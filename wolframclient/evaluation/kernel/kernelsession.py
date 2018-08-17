# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from subprocess import PIPE, Popen

from threading import Event, Thread

from wolframclient.evaluation.result import WolframKernelEvaluationResult
from wolframclient.exception import WolframKernelException
from wolframclient.language import wl
from wolframclient.serializers import export
from wolframclient.utils.api import futures, os, time, zmq
from wolframclient.utils.encoding import force_text
from wolframclient.utils import six

import logging

logger = logging.getLogger(__name__)

__all__ = ['WolframLanguageSession']

class KernelLogger(Thread):

    TO_PY_LOG_LEVEL = {
        1: logging.DEBUG,
        2: logging.INFO,
        3: logging.WARN,
        4: logging.FATAL
    }

    MAX_MESSAGE_BEFORE_QUIT = 32

    def __init__(self, socket, level=logging.WARN):
        if not isinstance(socket, Socket):
            raise ValueError('Expecting a Socket.')
        self.socket = socket
        self.socket._bind()
        logger.info('Initializing Kernel logger on socket ' + self.socket.uri)
        super(KernelLogger, self).__init__(name='wolframkernel-logger-%s:%s' % (self.socket.host, self.socket.port))
        self.logger = logging.getLogger('WolframKernel-%s:%s' % (self.socket.host, self.socket.port))
        self.logger.setLevel(level)
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
                logger.warning('The maximum number of messages to log after a session finishes has been reached. \
                Some messages may have been discarded.')
            try:
                self.socket.close()
            except:
                logger.fatal('Failed to close ZMQ logging socket.')

class WolframLanguageSession(object):
    """A session to a Wolfram Kernel enabling evaluation of Wolfram Language expressions.

    Start a new session and send an expression for evaluation::

        kernel = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'
        with WolframLanguageSession(kernel) as session:
            session.evaluate('Range[3]')

    Communication with a given kernel is based on ZMQ sockets:

    * one `PUSH` socket receiving expressions to evaluate,
    * one `PULL` socket to read evaluation results.

    Kernel logging is enabled by default and is done through a third socket
    (type `PULL`). The kernel associated to a given session provides the following
    logging functions:

    * ``ClientLibrary`debug`` corresponding to :py:meth:`logging.Logger.debug`
    * ``ClientLibrary`info`` corresponding to :py:meth:`logging.Logger.info`
    * ``ClientLibrary`warn`` corresponding to :py:meth:`logging.Logger.warning`
    * ``ClientLibrary`error`` corresponding to :py:meth:`logging.Logger.error`

    """

    def __init__(self, kernel=None, initfile=None,
                 in_socket=None, out_socket=None, kernel_loglevel=logging.NOTSET, logger_socket=None):
        if isinstance(kernel, six.string_types):
            if not os.isfile(kernel):
                raise WolframKernelException('Kernel not found at %s.' % kernel)
            elif not os.access(kernel, os.X_OK):
                raise WolframKernelException('Cannot execute kernel %s.' % kernel)
            else:
                self.kernel = kernel
        else:
            raise ValueError('Invalid kernel value. Expecting a filepath as a string.')
        if initfile is None:
            self.initfile = os.path_join(os.dirname(__file__), 'initkernel.m')
        else:
            self.initfile = initfile
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Initializing kernel %s using script: %s' % (self.kernel, self.initfile))
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

        if kernel_loglevel != logging.NOTSET:
            if logger_socket is None:
                self.logger_socket = Socket(zmq_type=zmq.PULL)
            elif isinstance(out_socket, Socket):
                logger_socket.zmq_type = zmq.PULL
                self.logger_socket = logger_socket
            else:
                raise ValueError(
                    'Expecting kernel logger socket to be a Socket instance.')

        self.kernel_proc = None
        self.loglevel = kernel_loglevel
        self.kernel_logger = None
        self.evaluation_count = 0
        self.thread_pool_exec = None
        self.parameters = {}

    _DEFAULT_PARAMETERS = {
        'STARTUP_READ_TIMEOUT': 20,
        'STARTUP_RETRY_SLEEP_TIME': 0.001,
        'TERMINATE_READ_TIMEOUT': 3,
    }

    def get_parameter(self, parameter_name):
        """Return the value of a given session parameter.

        Session parameters are:

        * ``'STARTUP_READ_TIMEOUT'``: time to wait, in seconds, after the kernel start-up was requested. Default is 20 seconds.
        * ``'STARTUP_RETRY_SLEEP_TIME'``: time to sleep, in seconds, before checking that the initilazed kernel has responded. Default is 1 ms.
        * ``'TERMINATE_READ_TIMEOUT'``: time to wait, in seconds, after the ``Quit[]`` command was sent to the kernel. The kernel is killed after this duration. Default is 3 seconds.
        """
        try:
            return self.parameters.get(parameter_name, self._DEFAULT_PARAMETERS.get(parameter_name))
        except KeyError:
            raise KeyError('%s has no such parameter %s' %
                           (self.__class__.__name__, parameter_name))

    def set_parameter(self, parameter_name, parameter_value):
        """Set a new value for a given parameter. The new value only applies for this session.

        Session parameters are:

        * ``'STARTUP_READ_TIMEOUT'``: time to wait, in seconds, after the kernel start-up was requested. Default is 20 seconds.
        * ``'STARTUP_RETRY_SLEEP_TIME'``: time to sleep, in seconds, before checking that the initilazed kernel has responded. Default is 1 ms.
        * ``'TERMINATE_READ_TIMEOUT'``: time to wait, in seconds, after the ``Quit[]`` command was sent to the kernel. The kernel is killed after this duration. Default is 3 seconds.
        """
        if parameter_name not in self._DEFAULT_PARAMETERS:
            raise KeyError('%s is not a valid parameter name.' % parameter_name)
        self.parameters[parameter_name] = parameter_value

    def __enter__(self):
        """Start the session."""
        self.start()
        return self

    def __exit__(self, type, value, traceback):
        """Terminate the kernel process and close sockets."""
        self.terminate()

    def terminate(self):
        """Terminate the kernel process and close sockets.

        This function must be called when a given session is no more useful
        to prevent orfan processes and sockets from being generated.

        .. note::
            Licencing restrictions usually apply to Wolfram kernels and may
            prevent new instances from starting if too many kernels are running
            simultaneously. Make sure to always terminate sessions to avoid
            unexpected start-up errors.
        """
        # Exception handling here
        if self.kernel_proc is not None:
            try:
                self.in_socket.zmq_socket.send(
                    b'8:f\x00s\x04Quit', flags=zmq.NOBLOCK)
                self.kernel_proc.stdout.close()
                self.kernel_proc.stdin.close()
                self.kernel_proc.terminate()
                self.kernel_proc.wait(timeout=self.get_parameter('TERMINATE_READ_TIMEOUT'))
            except:
                logger.warning('Failed to cleanly stop the kernel process after %.02f seconds. Killing it.' %
                               self.get_parameter('TERMINATE_READ_TIMEOUT'))
                self.kernel_proc.kill()
        if self.in_socket is not None:
            try:
                self.in_socket.close()
            except Exception as e:
                logger.fatal(e)
        if self.out_socket is not None:
            try:
                self.out_socket.close()
            except Exception as e:
                logger.fatal(e)
        if self.kernel_logger is not None:
            try:
                self.kernel_logger.stopped.set()
                self.kernel_logger.join()
            except Exception as e:
                logger.fatal(e)
        if self.thread_pool_exec is not None:
            try:
                self.thread_pool_exec.shutdown(wait=True)
            except Exception as e:
                logger.fatal(e)

    _KERNEL_OK = b'OK'

    def start(self):
        """Start a new kernel process and open sockets to communicate with it."""
        # start the evaluation zmq sockets
        self.in_socket._bind()
        self.out_socket._bind()
        # start the kernel process
        cmd = [self.kernel, '-noprompt', "-initfile", self.initfile]
        if self.loglevel is not None:
            self.kernel_logger = KernelLogger(
                socket=self.logger_socket, level=self.loglevel)
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
            self.kernel_proc = Popen(cmd, stdout=PIPE, stdin=PIPE)
            if logger.isEnabledFor(logging.INFO):
                logger.info('Kernel process started with PID: %s' % self.kernel_proc.pid)
                t_start = time.perf_counter()
        except Exception as e:
            logger.fatal(e)
            self.terminate()
            raise e
        try:
            # First message must be "OK", acknowledging everything is up and running
            # on the kernel side.
            response = self.out_socket._read_timeout(
                timeout=self.get_parameter('STARTUP_READ_TIMEOUT'),
                retry_sleep_time=self.get_parameter('STARTUP_RETRY_SLEEP_TIME'))
            if response == WolframLanguageSession._KERNEL_OK:
                if logger.isEnabledFor(logging.INFO):
                    logger.info(
                        'Kernel is ready. Startup took %.2f seconds.', time.perf_counter() - t_start)
            else:
                self._dump_pipe_to_log(self.kernel_proc.stdout)
                self.terminate()
                raise WolframKernelException('Kernel %s failed to start properly.' % self.kernel)
        except SocketException as se:
            logger.fatal(se)
            self._dump_pipe_to_log(self.kernel_proc.stdout)
            self.terminate()
            raise WolframKernelException('Failed to communication with the kernel %s. Could not read from ZMQ socket.' % self.kernel)

    @property
    def started(self):
        """Indicate if the current kernel session has started with reasonable accuracy."""
        return self.kernel_proc is not None

    def evaluate(self, expr, **kwargs):
        """Send an expression to the kernel for evaluation return a
        :class:`~wolframclient.evaluation.result.WolframKernelEvaluationResult`.

        The `expr` can be:

            * a text string representing the Wolfram Language expression :wl:`InputForm`.
            * an instance of Python object serializable as WXF by :func:`~wolframclient.serializers.export`.
            * a binary string of a serialized expression in the WXF format.

        `kwargs` are passed to :func:`~wolframclient.serializers.export` during serialization step of
        non-string inputs.
        """
        if not self.started:
            raise WolframKernelException('Kernel is not started.')

        if isinstance(expr, six.binary_type):
            self.in_socket.zmq_socket.send(expr)
        else:
            if isinstance(expr, six.string_types):
                expr = wl.ToExpression(expr)
            self.in_socket.zmq_socket.send(export(expr, target_format='wxf', **kwargs))
        # read the message as bytes.
        msg_count = self.out_socket.zmq_socket.recv()
        try:
            msg_count = int(msg_count)
        except ValueError:
            raise WolframKernelException('Unexpected message count returned by Kernel %s' % msg_count)
        errmsg = []
        for i in range(msg_count):
            errmsg.append(force_text(self.out_socket.zmq_socket.recv()))
        wxf_result = self.out_socket.zmq_socket.recv()
        self.evaluation_count += 1
        return WolframKernelEvaluationResult(wxf_result, errmsg)

    def evaluate_async(self, expr, **kwargs):
        """Evaluate a given `expr` asynchronously.

        Returns a :class:`concurrent.futures.Future` object.

        .. warning::
            Asynchronous evaluation is only available for `Python 3.2` and above.
        """
        try:
            if self.thread_pool_exec is None:
                self.thread_pool_exec = futures.ThreadPoolExecutor(max_workers=1)
            return self.thread_pool_exec.submit(self.evaluate, expr, **kwargs)
        except ImportError:
            logger.fatal('Module concurrent.futures is missing.')
            raise NotImplementedError('Asynchronous evaluation is not available on this Python interpreter.')

    def _dump_pipe_to_log(self, pipe):
        pass

    # def _dump_pipe_to_log(self, pipe):
    #     # this will probably fails on remote kernels (broken pipe?)
    #     try:
    #         for line in _non_blocking_pipe_readline(pipe):
    #             last = force_text(line.rstrip())
    #             logger.warning(last)
    #     except Exception as e:
    #         logger.warn('Exception occured while reading pipe: %s', e)

    def __repr__(self):
        return '<WolframLanguageSession: in:%s, out:%s>' % (self.in_socket.uri, self.out_socket.uri)

# def _non_blocking_pipe_readline(pipe):
#     ''' Read lines from a given `PIPE` in a non-blocking fashion.

#     Modifies the internal flags of the given pipe to do so. 
#     TODO DOES NOT WORK ON WINDOWS
#     May cause crashes
#     '''
#     import fcntl
#     from os import O_NONBLOCK
#     flags = fcntl.fcntl(pipe, fcntl.F_GETFL)
#     fcntl.fcntl(pipe, fcntl.F_SETFL, flags | O_NONBLOCK)
#     for line in pipe:
#         yield line

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

    def _read_timeout(self, timeout=2., retry_sleep_time=0.001):
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
                time.sleep(retry_sleep_time)
        raise SocketException('Read time out. Failed to read any message from socket %s after %.1f seconds and %i retries.'
                              % (self.uri, time.perf_counter() - start, retry))

    def close(self):
        self.zmq_socket.close()

    def __repr__(self):
        return '<Socket: host=%s, port=%s>' % (self.host, self.port)

# class WolframKernel(object):
#     ''' Represents a Wolfram Kernel executable.

#     The kernel may live on a distant machine in which case the host
#     address must be specified.
#     '''
#     def __init__(self, path=None, host='127.0.0.1'):
#         if path is None:
#             self.path = self._sensible_path()
#         else:
#             self.path = expandvars(expanduser(path))
#         self.host = host

#     def _sensible_path(self):
#         return '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'

#     def __repr__(self):
#         return '<WolframKernel %s on %s>' % (self.host, self.path)
