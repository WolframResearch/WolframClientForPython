# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from subprocess import PIPE, Popen

from threading import Event, Thread

from wolframclient.evaluation.result import WolframKernelEvaluationResult
from wolframclient.exception import WolframKernelException, WolframEvaluationException
from wolframclient.language import wl
from wolframclient.language.expression import WLSymbol
from wolframclient.serializers import export
from wolframclient.utils.api import futures, os, time, zmq, json
from wolframclient.utils.encoding import force_text
from wolframclient.utils import six
if six.WINDOWS:
    from subprocess import STARTUPINFO, STARTF_USESHOWWINDOW

import logging

logger = logging.getLogger(__name__)

__all__ = ['WolframLanguageSession', 'WolframLanguageAsyncSession']

class KernelLogger(Thread):

    TO_PY_LOG_LEVEL = {
        1: logging.DEBUG,
        2: logging.INFO,
        3: logging.WARN,
        4: logging.FATAL
    }

    MAX_MESSAGE_BEFORE_QUIT = 32
    
    def __init__(self, level=logging.WARN):
        self.socket = Socket(zmq_type=zmq.SUB)
        self.socket.bind()
        # Subscribe to all since we want all log messages.
        self.socket.zmq_socket.setsockopt(zmq.SUBSCRIBE, b'')
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
                        time.sleep(.01)
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

    Kernel logging is disabled by default and is done through a third socket
    (type `SUB`). The initial log level is specificed by parameter kernel_loglevel.
    If log level was not set at initialization, logging is not available for the entire
    session. 
    
    It is possible to pass ZMQ sockets to use instead of new one, but this is generally
    not recommanded, probably never necessary.

    The kernel associated to a given session provides the following
    logging functions:

    * ``ClientLibrary`debug`` corresponding to :py:meth:`logging.Logger.debug`
    * ``ClientLibrary`info`` corresponding to :py:meth:`logging.Logger.info`
    * ``ClientLibrary`warn`` corresponding to :py:meth:`logging.Logger.warning`
    * ``ClientLibrary`error`` corresponding to :py:meth:`logging.Logger.error`
    * ``ClientLibrary`SetDebugLogLevel[]`` send debug messages and above``
    * ``ClientLibrary`SetInfoLogLevel[]`` send info messages and above``
    * ``ClientLibrary`SetWarnLogLevel[]`` send warning messages and above``
    * ``ClientLibrary`SetErrorLogLevel[]`` only send error messages``
    * ``ClientLibrary`DisableKernelLogging[]`` stop sending error message to the logging socket``

    The standart input, output and error file handles can be specified with stdin, stdout and stderr
    named parameters. Valid values are those of subprocess.Popen. Those parameters should be handled
    with care as deadlocks can arise from misconfiguration.

    .. note :: 
        Wolfram Language sessions are **not thread-safe**, each thread must have its own instance.

    """

    def __init__(self, kernel=None, consumer=None, initfile=None,
                 in_socket=None, out_socket=None, kernel_loglevel=logging.NOTSET, stdin=PIPE, stdout=PIPE, stderr=PIPE):
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
        if not os.isfile(self.initfile):
            raise FileNotFoundError('Kernel initialization file %s not found.' % self.initfile)
        
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

        self.consumer = consumer
        self.kernel_proc = None
        self.terminated = False
        self.loglevel = kernel_loglevel
        self.kernel_logger = None
        self.evaluation_count = 0
        self.parameters = {}
        self._stdin = stdin
        self._stdout = stdout
        self._stderr = stderr
        

    _DEFAULT_PARAMETERS = {
        'STARTUP_READ_TIMEOUT': 20,
        'STARTUP_RETRY_SLEEP_TIME': 0.001,
        'TERMINATE_READ_TIMEOUT': 3,
        'HIDE_SUBPROCESS_WINDOW': True
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
                if six.PY2:
                    self.kernel_proc.wait()
                else:
                    self.kernel_proc.wait(timeout=self.get_parameter('TERMINATE_READ_TIMEOUT'))
                if self._stdin == PIPE:
                    self.kernel_proc.stdin.close()
                if self._stdout == PIPE:
                    self.kernel_proc.stdout.close()
                if self._stderr == PIPE:
                    self.kernel_proc.stderr.close()
            except Exception as e:
                if six.PY3:
                    logger.warning('Failed to cleanly stop the kernel process after %.02f seconds. Killing it.' % self.get_parameter('TERMINATE_READ_TIMEOUT'))
                else:
                    logger.warning('Failed to cleanly stop the kernel process. Killing it.')
                logger.warning('An exception occured: %s.' % e)
                self.kernel_proc.kill()
            finally:
                self.terminated = True
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

    _KERNEL_OK = b'OK'

    def start(self):
        """Start a new kernel process and open sockets to communicate with it."""
        if self.terminated:
            raise WolframKernelException('Session has been terminated.')
        # start the evaluation zmq sockets
        self.in_socket.bind()
        self.out_socket.bind()
        if logger.isEnabledFor(logging.INFO):
            logger.info('Kernel receives commands from socket: %s', self.in_socket)
            logger.info('Kernel writes evaluated expressions to socket: %s', self.out_socket)
        # start the kernel process
        cmd = [self.kernel, '-noprompt', "-initfile", self.initfile]
        if self.loglevel != logging.NOTSET:
            self.kernel_logger = KernelLogger(level=self.loglevel)
            self.kernel_logger.start()
            cmd.append('-run')
            cmd.append('ClientLibrary`Private`SlaveKernelPrivateStart["%s", "%s", "%s"];'
                       % (self.in_socket.uri, self.out_socket.uri, self.kernel_logger.socket.uri))
        else:
            cmd.append('-run')
            cmd.append('ClientLibrary`Private`SlaveKernelPrivateStart["%s", "%s"];'
                       % (self.in_socket.uri, self.out_socket.uri))

        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Kernel called using command: %s.' % ' '.join(cmd))
        # hide the WolframKernel window.
        if six.WINDOWS and self.get_parameter('HIDE_SUBPROCESS_WINDOW'):
            startupinfo = STARTUPINFO()
            startupinfo.dwFlags |= STARTF_USESHOWWINDOW
        else:
            startupinfo = None
        try:
            self.kernel_proc = Popen(cmd, stdin=self._stdin, stdout=self._stdout, stderr=self._stderr, startupinfo=startupinfo)
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
            response = self.out_socket.read_timeout(
                timeout=self.get_parameter('STARTUP_READ_TIMEOUT'),
                retry_sleep_time=self.get_parameter('STARTUP_RETRY_SLEEP_TIME'))
            if response == WolframLanguageSession._KERNEL_OK:
                if logger.isEnabledFor(logging.INFO):
                    logger.info(
                        'Kernel is ready. Startup took %.2f seconds.', time.perf_counter() - t_start)
            else:
                self.terminate()
                raise WolframKernelException('Kernel %s failed to start properly.' % self.kernel)
        except SocketException as se:
            logger.fatal(se)
            self.terminate()
            raise WolframKernelException('Failed to communication with the kernel %s. Could not read from ZMQ socket.' % self.kernel)

    @property
    def started(self):
        """Indicate if the current kernel session has started with reasonable accuracy."""
        return self.kernel_proc is not None

    def _evaluate(self, expr, wrap_result=False, **kwargs):
        """Send an expression to the kernel for evaluation. Return a :class:`~wolframclient.evaluation.result.WolframKernelEvaluationResult`.
        """
        if not self.started:
            raise WolframKernelException('Kernel is not started.')
        if self.terminated:
            raise WolframKernelException('Session has been terminated.')
        start = time.perf_counter()
        if isinstance(expr, six.binary_type):
            logger.info('Expression is already serialized in WXF.')
            self.in_socket.zmq_socket.send(expr)
        else:
            if isinstance(expr, six.string_types):
                expr = wl.ToExpression(expr)
            self.in_socket.zmq_socket.send(export(expr, target_format='wxf', **kwargs))
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Expression sent to kernel in %.06fsec', time.perf_counter()-start)
            start = time.perf_counter()
        # read the message as bytes.
        msg_count = self.out_socket.zmq_socket.recv()
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Message count received from kernel after %.06fsec', time.perf_counter()-start)
        try:
            msg_count = int(msg_count)
        except ValueError:
            raise WolframKernelException('Unexpected message count returned by Kernel %s' % msg_count)
        errmsg = []
        for i in range(msg_count):
            json_msg = self.out_socket.zmq_socket.recv_json()
            errmsg.append((json_msg[0], force_text(json_msg[1])))
        wxf_result = self.out_socket.zmq_socket.recv()
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Expression received from kernel after %.06fsec', time.perf_counter()-start)
        self.evaluation_count += 1
        return WolframKernelEvaluationResult(wxf_result, errmsg, consumer=self.consumer)

    def evaluate_wxf(self, expr, **kwargs):
        """Send an expression to the kernel for evaluation and return the raw result still encoded as WXF.
        """
        result = self._evaluate(expr, **kwargs)
        if not result.success:
            for msg in result.messages:
                logger.warning(msg[1])
        return result.wxf

    def evaluate_wrap(self, expr, **kwargs):
        """ Similar to :func:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate` but return the result as a :class:`~wolframclient.evaluation.result.WolframKernelEvaluationResult`.
        """
        return self._evaluate(expr, **kwargs)
    
    def evaluate(self, expr, **kwargs):
        """Send an expression to the kernel for evaluation.

        The `expr` can be:

            * a text string representing the Wolfram Language expression :wl:`InputForm`.
            * an instance of Python object serializable as WXF by :func:`~wolframclient.serializers.export`.
            * a binary string of a serialized expression in the WXF format.

        `kwargs` are passed to :func:`~wolframclient.serializers.export` during serialization step of
        non-string inputs.
        """
        result = self._evaluate(expr, kwargs)
        if not result.success:
            for msg in result.messages:
                logger.warning(msg[1])
        return result.get()

    def __getattr__(self, attr):
        def inner(*args, **kwargs):
            expr = WLSymbol(force_text(attr))(*args, **kwargs)
            return self.evaluate(expr)
        return inner

    def __repr__(self):
        if self.terminated:
            return '<%s: terminated>' % self.__class__.__name__
        elif self.started:
            return '<%s: pid:%i, kernel sockets: (in:%s, out:%s)>' % (self.__class__.__name__, self.kernel_proc.pid, self.in_socket.uri, self.out_socket.uri)
        else:
            return '<%s: not started>' % self.__class__.__name__

class WolframLanguageAsyncSession(WolframLanguageSession):
    """Evaluate expression asynchronously.

    Evaluation methods of this class returns a :class:`~concurrent.futures.Future` objects.

    .. warning::
        Asynchronous evaluation is only available for `Python 3.2` and above.
    """

    def __init__(self, kernel=None, consumer=None, initfile=None,
                 in_socket=None, out_socket=None, kernel_loglevel=logging.NOTSET, stdin=PIPE, stdout=PIPE, stderr=PIPE):
        super(WolframLanguageAsyncSession, self).__init__(kernel, consumer, initfile,
            in_socket, out_socket, kernel_loglevel, stdin, stdout, stderr)
        self.thread_pool_exec = None

    def evaluate(self, expr, **kwargs):
        return self._do_async(super().evaluate, expr, **kwargs)

    def evaluate_wxf(self, expr, **kwargs):
        return self._do_async(super().evaluate_wxf, expr, **kwargs)

    def evaluate_wrap(self, expr, **kwargs):
        return self._do_async(super().evaluate_wrap, expr, **kwargs)
    
    def _do_async(self, func, expr, **kwargs):
        try:
            if self.thread_pool_exec is None:
                self.thread_pool_exec = futures.ThreadPoolExecutor(
                    max_workers=1)
            return self.thread_pool_exec.submit(func, expr, **kwargs)
        except ImportError:
            logger.fatal('Module concurrent.futures is missing.')
            raise NotImplementedError(
                'Asynchronous evaluation is not available on this Python interpreter.')

    def terminate(self):
        # First terminate all executions.
        # Then use the socket to actually quit. Avoid crashes when freeing zmq resources still in use.
        if self.thread_pool_exec is not None:
            try:
                self.thread_pool_exec.shutdown(wait=True)
            except Exception as e:
                logger.fatal(e)
        super().terminate()

    def __getattr__(self, attr):
        def inner(*args, **kwargs):
            expr = WLSymbol(force_text(attr))(*args, **kwargs)
            return self.evaluate(expr)
        return inner


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
        self.closed = False

    def bind(self):
        if self.bound:
            raise SocketException('Already bounded.')
        if self.closed:
            raise SocketException('Socket has been closed.')
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

    def read_timeout(self, timeout=2., retry_sleep_time=0.001):
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
        self.closed = True
        
    def __repr__(self):
        if self.bound:
            return '<Socket: host=%s, port=%s>' % (self.host, self.port)
        else:
            return '<Socket (not bounded): host=%s>' % self.host
