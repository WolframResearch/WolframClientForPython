from __future__ import absolute_import, print_function
import logging
from math import floor
from os.path import expandvars, expanduser, dirname, join as path_join
from subprocess import Popen, PIPE
from threading import Thread, Event
from wolframclient.utils.six import string_types, binary_type, integer_types, PY3, JYTHON
if PY3:
    from concurrent.futures import ThreadPoolExecutor
from wolframclient.serializers.export import export
from wolframclient.language.expression import wl
from wolframclient.exception import WolframKernelException
from wolframclient.utils.encoding import force_text
from wolframclient.utils.api import zmq, time
if not JYTHON:
    from wolframclient.evaluation.evaluationresult import WolframEvaluationResult

logger = logging.getLogger(__name__)


__all__ = ['WolframLanguageSession', 'WolframKernel']


class KernelLogger(Thread):

    TO_PY_LOG_LEVEL = {
        1: logging.DEBUG,
        2: logging.INFO,
        3: logging.WARN,
        4: logging.FATAL
    }

    MAX_MESSAGE_BEFORE_QUIT = 32

    def __init__(self, socket):
        if not isinstance(socket, Socket):
            raise ValueError('Expecting a Socket.')
        self.socket = socket
        self.socket._bind()
        logger.info('Initializing Kernel logger on socket ' + self.socket.uri)
        super(KernelLogger, self).__init__(name='wolframkernel-logger-%s:%s' % (self.socket.host, self.socket.port))
        self.logger = logging.getLogger('WolframKernel-%s:%s' % (self.socket.host, self.socket.port))
        self.stopped = Event()

    def run(self):
        logger.debug('Start receiving kernel logger messages.')
        msg_after_quit = 0
        zmq_socket = self.socket.zmq_socket
        try:
            while msg_after_quit < KernelLogger.MAX_MESSAGE_BEFORE_QUIT:
                try:
                    msg = zmq_socket.recv_json(flags=zmq.NOBLOCK)
                    level = KernelLogger.TO_PY_LOG_LEVEL.get(msg.get('level', 3))
                    msg_text = msg.get('msg', 'Malformed kernel message. Missing key "msg".')
                    self.logger.log(level, msg_text)
                    if self.stopped.is_set():
                        msg_after_quit += 1
                except zmq.Again:
                    if self.stopped.is_set():
                        break
                    else:
                        time.sleep(.1)
        # no matter what we try to close the socket:
        finally:
            logger.info('Terminating kernel logger thread.')
            if msg_after_quit == KernelLogger.MAX_MESSAGE_BEFORE_QUIT:
                logger.warn('The maximum number of messages to log after a session finishes has been reached. \
                Some messages may have been discarded.')
            try:
                self.socket.close()
            except:
                logger.fatal('Failed to close ZMQ logging socket.')


class WolframLanguageSession(object):
    ''' A session to a Wolfram Kernel enabling evaluation of Wolfram
    Language expressions.

    Example
    -----------

    Start a new session and send a expression for evaluation::

        from  wolframclient.evaluation import WolframKernel, WolframLanguageSession

        kernel = WolframKernel('/Applications/Wolfram\ Desktop.app/Contents/MacOS/WolframKernel')
        with WolframLanguageSession(kernel) as session:
            session.evaluate('Range[3]')

    Communicates with a given kernel using ZMQ sockets:

    * one `PUSH` socket receiving expressions to evaluate,
    * one `PULL` socket to read evaluation results.

    Kernel logging is enabled by default and is done through a third socket
    (PULL). The kernel associated to a given session provides the following
    logging functions:

    * ``ClientLibrary`debug`` corresponding to :py:meth:`logging.Logger.debug`
    * ``ClientLibrary`info`` corresponding to :py:meth:`logging.Logger.info`
    * ``ClientLibrary`warn`` corresponding to :py:meth:`logging.Logger.warn`
    * ``ClientLibrary`error`` corresponding to :py:meth:`logging.Logger.error`

    '''
    def __init__(self, kernel=None, initfile=None, log_kernel=True,
                 in_socket=None, out_socket=None, logger_socket=None):
        if kernel is None:
            self.kernel = WolframKernel()
        elif isinstance(kernel, string_types):
            self.kernel = WolframKernel(kernel)
        elif isinstance(kernel, WolframKernel):
            self.kernel = kernel
        else:
            raise ValueError(
                'Invalid kernel value. Expecting a filepath as a string or an instance of WolframKernel.')

        if initfile is None:
            self.initfile = path_join(dirname(__file__), 'initkernel.m')
        else:
            self.initfile = initfile
        logger.debug('Initializing kernel using script: %s', self.initfile)
        # Socket to which we push new expressions to evaluate. 
        if in_socket is None:
            self.in_socket = Socket(zmq_type=zmq.PUSH)
        elif not isinstance(in_socket, Socket):
            raise ValueError('Expecting kernel input socket to be a Socket instance.')
        else:
            in_socket.zmq_type = zmq.PUSH
            self.in_socket = in_socket

        if out_socket is None:
            self.out_socket = Socket(zmq_type=zmq.PULL)
        elif not isinstance(out_socket, Socket):
            raise ValueError('Expecting kernel output socket to be a Socket instance.')
        else:
            out_socket.zmq_type = zmq.PULL
            self.out_socket = out_socket

        if logger.isEnabledFor(logging.INFO):
            logger.info('Kernel receive commands to socket: %s', self.in_socket.uri)
            logger.info('Kernel write evaluated expressions to socket: %s', self.out_socket.uri)

        if logger_socket is None:
            self.logger_socket = Socket(zmq_type=zmq.PULL)
        elif not isinstance(out_socket, Socket):
            raise ValueError('Expecting kernel logger socket to be a Socket instance.')
        else:
            logger_socket.zmq_type = zmq.PULL
            self.logger_socket = logger_socket

        self.kernel_proc = None
        self.log = log_kernel
        self.kernel_logger = None
        self.evaluation_count = 0
        self.thread_pool_exec = None

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, type, value, traceback):
        self.terminate()

    def terminate(self):
        logger.debug('Cleanning up kernel session.')
        # Exception handling here
        if self.kernel_proc is not None:
            try:
                self.in_socket.zmq_socket.send_string('Quit[]')
                self.kernel_proc.stdout.close()
                self.kernel_proc.stdin.close()
                self.kernel_proc.terminate()
                self.kernel_proc.wait(timeout=3)
            except:
                logger.warning('Failed to cleanly stop the kernel process. Killing it.')
                self.kernel_proc.kill()
        if self.in_socket is not None:
            self.in_socket.close()
        if self.out_socket is not None:
            self.out_socket.close()
        if self.kernel_logger is not None:
            self.kernel_logger.stopped.set()
            self.kernel_logger.join()
        if self.thread_pool_exec is not None:
            self.thread_pool_exec.shutdown(wait=True)

    KERNEL_OK = b'OK'

    def start(self):
        # start the evaluation zmq sockets
        self.in_socket._bind()
        self.out_socket._bind()
        # start the kernel process
        cmd = [self.kernel.path, '-noprompt', "-initfile", self.initfile]
        if self.log:
            self.kernel_logger = KernelLogger(socket=self.logger_socket)
            self.kernel_logger.start()
            cmd.append('-run')
            cmd.append('ClientLibrary`Private`SlaveKernelPrivateStart["%s", "%s", "%s"];'
                       % (self.in_socket.uri, self.out_socket.uri, self.logger_socket.uri))
        else:
            cmd.append('-run')
            cmd.append('ClientLibrary`Private`SlaveKernelPrivateStart["%s", "%s"];'
                       % (self.in_socket.uri, self.out_socket.uri))

        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Kernel called using command: %s.' % ' '.join(cmd))

        try:
            # from asyncio import create_subprocess_exec
            # (self.kernel_proc, _) = await create_subprocess_exec(*cmd, stdout=PIPE)

            self.kernel_proc = Popen(cmd, stdout=PIPE, stdin=PIPE)
            if logger.isEnabledFor(logging.INFO):
                logger.info('Kernel process started with PID: %s' % self.kernel_proc.pid)
                # logger.info('Kernel process started with PID: %s' % self.kernel_proc.get_pid())
                t_start = time.perf_counter()
            # First message must be "OK", acknowledging everything is up and running 
            # on the kernel side.
            response = self.out_socket._read_timeout(timeout=20)
            if response == WolframLanguageSession.KERNEL_OK:
                if logger.isEnabledFor(logging.INFO):
                    logger.info(
                        'Kernel is ready. Startup took %.2f seconds.', time.perf_counter() - t_start)
            else:
                # this will probably fails on remote kernels (broken pipe?)
                for line in _non_blocking_pipe_readline(self.kernel_proc.stdout):
                # for line in _non_blocking_pipe_readline(self.kernel_proc.get_pipe_transport(1)):
                    last = force_text(line.rstrip())
                    logger.warn(last)
                raise WolframKernelException(
                    'Failed to establish communication with the kernel. See log for more information.')
        except SocketException as se:
            logger.fatal(se)
            self.terminate()
            raise WolframKernelException(
                'Failed to communication with the kernel. Could not read from ZMQ socket.')
        except Exception as e:
            logger.fatal(e)
            self.terminate()
            raise e

    @property
    def started(self):
        return self.kernel_proc is not None

    def evaluate(self, expr, **kwargs):
        logger.debug('new evaluation on: %s', self)
        if not self.started:
            raise WolframKernelException('Kernel is not started.')

        if isinstance(expr, string_types):
            self.in_socket.zmq_socket.send_string(expr)
        elif isinstance(expr, binary_type):
            self.in_socket.zmq_socket.send(expr)
        else:
            self.in_socket.zmq_socket.send(export(expr, **kwargs))
        # read the message as bytes.
        msgstr = self.out_socket.zmq_socket.recv()
        self.evaluation_count += 1
        return WolframEvaluationResult(expr=msgstr)

    if PY3:
        def evaluate_async(self, expr, **kwargs):
            if self.thread_pool_exec is None:
                self.thread_pool_exec = ThreadPoolExecutor(max_workers=1)

            return self.thread_pool_exec.submit(self.evaluate, expr, **kwargs)

    def __repr__(self):
        return '<WolframLanguageSession: in:%s, out:%s>' % (self.in_socket.uri, self.out_socket.uri)


def _non_blocking_pipe_readline(pipe):
    ''' Read lines from a given `PIPE` in a non-blocking fashing.

    Modifies the internal flags of the given pipe to do so.
    '''
    import fcntl
    from os import O_NONBLOCK
    flags = fcntl.fcntl(pipe, fcntl.F_GETFL)
    fcntl.fcntl(pipe, fcntl.F_SETFL, flags | O_NONBLOCK)
    for line in pipe:
        yield line


class SocketException(Exception):
    pass


class Socket(object):
    def __init__(self, host='127.0.0.1', port=None, zmq_type=zmq.PAIR):
        self.host = host
        self.port = port
        if self.port is None:
            self.uri = 'tcp://' + self.host
        else:
            self.uri = 'tcp://%s:%s' % (self.host, self.port)
        self.zmq_type = zmq_type
        self.bound = False
        self.zmq_socket = self.zmq_socket = zmq.Context.instance().socket(zmq_type)

    def _bind(self):
        if self.bound:
            raise SocketException('Already bounded.')
        self.zmq_socket = zmq.Context.instance().socket(self.zmq_type)
        if self.port is None:
            logger.debug('Binding socket using random port.')
            self.port = self.zmq_socket.bind_to_random_port(self.uri)
            self.uri = 'tcp://%s:%s' % (self.host, self.port)
        else:
            self.zmq_socket.bind(self.uri)
        logger.debug('ZMQ socket bound to ' + self.uri)
        self.bound = True
        return self.zmq_socket

    RETRY_SLEEP_TIME = 0.01

    def _read_timeout(self, timeout=2.):
        if not self.bound:
            raise SocketException('ZMQ socket not bound.')
        if timeout < 0:
            raise ValueError('Timeout must be a positive number.')
        retry = 0
        start = time.perf_counter()
        while time.perf_counter() - start < timeout:
            try:
                return self.zmq_socket.recv(flags=zmq.NOBLOCK)
            except zmq.Again:
                retry += 1
                time.sleep(Socket.RETRY_SLEEP_TIME)
        raise SocketException('Read time out. Failed to read any message from socket %s after %.1f seconds and %i retries.'
                              % (self.uri, time.perf_counter() - start, retry))

    def close(self):
        self.zmq_socket.close()

    def __repr__(self):
        return '<Socket: host=%s, port=%s>' % (self.host, self.port)


class WolframKernel(object):
    ''' Represents a Wolfram Kernel executable.

    The kernel may live on a distant machine in which case the host
    address must be specified.
    '''
    def __init__(self, path=None, host='127.0.0.1'):
        if path is None:
            self.path = self._sensible_path()
        else:
            self.path = expandvars(expanduser(path))
        self.host = host

    def _sensible_path(self):
        return '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'

    def __repr__(self):
        return '<WolframKernel %s on %s>' % (self.host, self.path)
