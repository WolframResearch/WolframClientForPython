# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
from subprocess import PIPE, Popen
from threading import Event, Thread

from wolframclient.evaluation.base import WolframEvaluator
from wolframclient.evaluation.result import WolframKernelEvaluationResult
from wolframclient.exception import WolframKernelException
from wolframclient.serializers import export
from wolframclient.utils import six
from wolframclient.utils.api import json, os, time, zmq
from wolframclient.utils.encoding import force_text

if six.WINDOWS:
    from subprocess import STARTUPINFO, STARTF_USESHOWWINDOW

logger = logging.getLogger(__name__)

__all__ = ['WolframLanguageSession']

TO_PY_LOG_LEVEL = {
    1: logging.DEBUG,
    2: logging.INFO,
    3: logging.WARN,
    4: logging.FATAL
}
FROM_PY_LOG_LEVEL = dict((v, k) for k, v in TO_PY_LOG_LEVEL.items())


class KernelLogger(Thread):
    """ Asynchronous logger for kernel messages. 
    
    A consumer of messages read from a PUB/SUB socket that turn them into log messages as expected
    by the :mod:`logging` module.
    """
    MAX_MESSAGE_BEFORE_QUIT = 32

    def __init__(self, level=logging.WARN):
        self.socket = Socket(zmq_type=zmq.SUB)
        self.socket.bind()
        # Subscribe to all since we want all log messages.
        self.socket.zmq_socket.setsockopt(zmq.SUBSCRIBE, b'')
        logger.info('Initializing Kernel logger on socket ' + self.socket.uri)
        super(KernelLogger,
              self).__init__(name='wolframkernel-logger-%s:%s' %
                             (self.socket.host, self.socket.port))
        self.logger = logging.getLogger(
            'WolframKernel-%s:%s' % (self.socket.host, self.socket.port))
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
                    level = TO_PY_LOG_LEVEL.get(msg.get('level', 3))
                    msg_text = msg.get(
                        'msg', 'Malformed kernel message. Missing key "msg".')
                    self.logger.log(level, msg_text)
                    if self.stopped.is_set():
                        msg_after_quit += 1
                except zmq.Again:
                    if self.stopped.is_set():
                        break
                    else:
                        time.sleep(.01)
                except json.JSONDecodeError as e:
                    logger.warning('Invalid message: %s', e.doc)
        # no matter what we try to close the socket:
        finally:
            logger.info('Terminating kernel logger thread.')
            if msg_after_quit == KernelLogger.MAX_MESSAGE_BEFORE_QUIT:
                logger.warning(
                    'The maximum number of messages to log after a session finishes has been reached. \
                Some messages may have been discarded.')
            try:
                self.socket.close()
            except:
                logger.fatal('Failed to close ZMQ logging socket.')


class WolframLanguageSession(WolframEvaluator):
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
    * ``ClientLibrary`SetDebugLogLevel[]`` send debug messages and above
    * ``ClientLibrary`SetInfoLogLevel[]`` send info messages and above
    * ``ClientLibrary`SetWarnLogLevel[]`` send warning messages and above
    * ``ClientLibrary`SetErrorLogLevel[]`` only send error messages
    * ``ClientLibrary`DisableKernelLogging[]`` stop sending error message to the logging socket

    The standart input, output and error file handles can be specified with `stdin`, `stdout` and `stderr`
    named parameters. Valid values are those accepted by subprocess.Popen (e.g :data:`sys.stdout`). Those parameters should be handled
    with care as deadlocks can arise from misconfiguration.

    .. note ::
        Wolfram Language sessions are **not thread-safe**, each thread must run its own instance.

    """

    def __init__(self,
                 kernel,
                 consumer=None,
                 initfile=None,
                 in_socket=None,
                 out_socket=None,
                 kernel_loglevel=logging.NOTSET,
                 stdin=PIPE,
                 stdout=PIPE,
                 stderr=PIPE,
                 inputform_string_evaluation=True,
                 wxf_bytes_evaluation=True,
                 **kwargs):
        super().__init__(
            inputform_string_evaluation=inputform_string_evaluation)
        if isinstance(kernel, six.string_types):
            if not os.isfile(kernel):
                raise WolframKernelException(
                    'Kernel not found at %s.' % kernel)
            elif not os.access(kernel, os.X_OK):
                raise WolframKernelException(
                    'Cannot execute kernel %s.' % kernel)
            else:
                self.kernel = kernel
        else:
            raise ValueError(
                'Invalid kernel value. Expecting a filepath as a string.')
        if initfile is None:
            self.initfile = os.path_join(os.dirname(__file__), 'initkernel.m')
        else:
            self.initfile = initfile
        if not os.isfile(self.initfile):
            raise FileNotFoundError(
                'Kernel initialization file %s not found.' % self.initfile)

        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Initializing kernel %s using script: %s' %
                         (self.kernel, self.initfile))
        if in_socket is not None and not isinstance(in_socket, Socket):
            raise ValueError(
                'Expecting kernel input socket to be a Socket instance.')
        if out_socket is not None and not isinstance(out_socket, Socket):
            raise ValueError(
                'Expecting kernel output socket to be a Socket instance.')
        self.out_socket = out_socket
        self.in_socket = in_socket
        self.consumer = consumer
        self.kernel_proc = None
        self.loglevel = kernel_loglevel
        self.kernel_logger = None
        self.evaluation_count = 0
        self.parameters = {}
        self._stdin = stdin
        self._stdout = stdout
        self._stderr = stderr
        self.wxf_bytes_evaluation = wxf_bytes_evaluation
        # some parameters may be passed as kwargs
        for k, v in kwargs.items():
            self.set_parameter(k, v)

    def duplicate(self, session):
        """ Build a new object using the same configuration of the current one. """
        return WolframLanguageSession(
            self.kernel,
            consumer=self.consumer,
            initfile=self.initfile,
            in_socket=self.in_socket,
            out_socket=self.out_socket,
            kernel_loglevel=self.loglevel,
            stdin=self._stdin,
            stdout=self._stdout,
            stderr=self._stderr,
            inputform_string_evaluation=self.inputform_string_evaluation,
            wxf_bytes_evaluation=self.wxf_bytes_evaluation,
            **self.parameters)

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
            return self.parameters.get(
                parameter_name, self._DEFAULT_PARAMETERS.get(parameter_name))
        except KeyError:
            raise KeyError(
                '%s is not one of the valid parameters: %s' %
                (parameter_name, ', '.join(self._DEFAULT_PARAMETERS.keys())))

    def set_parameter(self, parameter_name, parameter_value):
        """Set a new value for a given parameter. The new value only applies for this session.

        Session parameters are:

        * ``'STARTUP_READ_TIMEOUT'``: time to wait, in seconds, after the kernel start-up was requested. Default is 20 seconds.
        * ``'STARTUP_RETRY_SLEEP_TIME'``: time to sleep, in seconds, before checking that the initilazed kernel has responded. Default is 1 ms.
        * ``'TERMINATE_READ_TIMEOUT'``: time to wait, in seconds, after the ``Quit[]`` command was sent to the kernel. The kernel is killed after this duration. Default is 3 seconds.
        """
        if parameter_name not in self._DEFAULT_PARAMETERS:
            raise KeyError(
                '%s is not one of the valid parameters: %s' %
                (parameter_name, ', '.join(self._DEFAULT_PARAMETERS.keys())))
        self.parameters[parameter_name] = parameter_value

    def terminate(self):
        """Immediatly stop the current session."""
        self._stop(gracefully=False)

    def stop(self):
        self._stop(gracefully=True)

    def _stop(self, gracefully=True):
        """Stop the kernel process and close sockets.

        This function must be called when a given session is no more useful
        to prevent orfan processes and sockets from being generated.

        .. note::
            Licencing restrictions usually apply to Wolfram kernels and may
            prevent new instances from starting if too many kernels are running
            simultaneously. Make sure to always terminate sessions to avoid
            unexpected start-up errors.
        """
        logger.info('Start termination on kernel %s', self)
        self.stopped = True
        if self.kernel_proc is not None:
            error = False
            if gracefully:
                # Graceful stop: first send a Quit command to the kernel.
                try:
                    self.in_socket.zmq_socket.send(
                        b'8:f\x00s\x04Quit', flags=zmq.NOBLOCK)
                except:
                    logger.info('Failed to send Quit[] command to the kernel.')
                    error = True
                if not error:
                    try:
                        self.kernel_proc.wait(
                            timeout=self.get_parameter(
                                'TERMINATE_READ_TIMEOUT'))
                    except:
                        logger.info(
                            'Kernel process failed to stop after %.02f seconds. Killing it.'
                            % self.get_parameter('TERMINATE_READ_TIMEOUT'))
                        error = True
                # Kill process if not already terminated.
                # Wait for it to cleanly stop if the Quit command was succesfully sent,
                # otherwise the kernel is likely in a bad state so we kill it immediatly.
            if self._stdin == PIPE:
                try:
                    self.kernel_proc.stdin.close()
                except:
                    logger.warning('Failed to close kernel process stdin.')
                    error = True
            if self._stdout == PIPE:
                try:
                    self.kernel_proc.stdout.close()
                except:
                    logger.warning('Failed to close kernel process stdout.')
                    error = True
            if self._stderr == PIPE:
                try:
                    self.kernel_proc.stderr.close()
                except:
                    logger.warning('Failed to close kernel process stderr')
                    error = True
            if error or not gracefully:
                logger.info(
                    'Killing kernel process: %i' % self.kernel_proc.pid)
                self.kernel_proc.kill()
            self.kernel_proc = None
        if self.in_socket is not None:
            try:
                self.in_socket.close()
            except Exception as e:
                logger.fatal(e)
            finally:
                self.in_socket = None
        if self.out_socket is not None:
            try:
                self.out_socket.close()
            except Exception as e:
                logger.fatal(e)
            finally:
                self.out_socket = None
        if self.kernel_logger is not None:
            try:
                self.kernel_logger.stopped.set()
                self.kernel_logger.join()
            except Exception as e:
                logger.fatal(e)
            finally:
                self.kernel_logger = None
        assert (self.kernel_proc is None)
        assert (self.out_socket is None)
        assert (self.in_socket is None)
        assert (self.kernel_logger is None)

    _socket_read_sleep_func = time.sleep

    _KERNEL_OK = b'OK'

    @property
    def started(self):
        return (self.kernel_proc is not None and self.in_socket
                and self.in_socket.bound and self.out_socket
                and self.out_socket.bound)

    def start(self):
        try:
            self._start()
        except Exception as e:
            try:
                self.terminate()
            finally:
                assert (self.stopped)
                assert (not self.started)
                raise e

    def _start(self):
        """Start a new kernel process and open sockets to communicate with it."""
        self.stopped = False
        if self.started:
            return
        # Socket to which we push new expressions for evaluation.
        if self.in_socket is None:
            self.in_socket = Socket(zmq_type=zmq.PUSH)
        else:
            self.in_socket.zmq_type = zmq.PUSH
        if self.out_socket is None:
            self.out_socket = Socket(zmq_type=zmq.PULL)
        else:
            self.out_socket.zmq_type = zmq.PULL
        # start the evaluation zmq sockets
        self.in_socket.bind()
        self.out_socket.bind()
        if logger.isEnabledFor(logging.INFO):
            logger.info('Kernel receives commands from socket: %s',
                        self.in_socket)
            logger.info('Kernel writes evaluated expressions to socket: %s',
                        self.out_socket)
        # start the kernel process
        cmd = [self.kernel, '-noprompt', "-initfile", self.initfile]
        if self.loglevel != logging.NOTSET:
            self.kernel_logger = KernelLogger(level=self.loglevel)
            self.kernel_logger.start()
            cmd.append('-run')
            cmd.append(
                'ClientLibrary`Private`SlaveKernelPrivateStart["%s", "%s", "%s", %i];'
                % (self.in_socket.uri, self.out_socket.uri,
                   self.kernel_logger.socket.uri,
                   FROM_PY_LOG_LEVEL[self.loglevel]))
        else:
            cmd.append('-run')
            cmd.append(
                'ClientLibrary`Private`SlaveKernelPrivateStart["%s", "%s"];' %
                (self.in_socket.uri, self.out_socket.uri))

        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Kernel called using command: %s.' % ' '.join(cmd))
        # hide the WolframKernel window.
        if six.WINDOWS and self.get_parameter('HIDE_SUBPROCESS_WINDOW'):
            startupinfo = STARTUPINFO()
            startupinfo.dwFlags |= STARTF_USESHOWWINDOW
        else:
            startupinfo = None
        try:
            self.kernel_proc = Popen(
                cmd,
                stdin=self._stdin,
                stdout=self._stdout,
                stderr=self._stderr,
                startupinfo=startupinfo)
            if logger.isEnabledFor(logging.INFO):
                logger.info('Kernel process started with PID: %s' %
                            self.kernel_proc.pid)
                t_start = time.perf_counter()
        except Exception as e:
            logger.exception(e)
            raise WolframKernelException('Failed to start kernel process.')
        try:
            # First message must be "OK", acknowledging everything is up and running
            # on the kernel side.
            response = self.out_socket.read_timeout(
                timeout=self.get_parameter('STARTUP_READ_TIMEOUT'),
                retry_sleep_time=self.get_parameter(
                    'STARTUP_RETRY_SLEEP_TIME'),
                sleep=self._socket_read_sleep_func)
            if response == WolframLanguageSession._KERNEL_OK:
                if logger.isEnabledFor(logging.INFO):
                    logger.info(
                        'Kernel %s is ready. Startup took %.2f seconds.' %
                        (self.pid, time.perf_counter() - t_start))
            else:
                raise WolframKernelException(
                    'Kernel %s failed to start properly.' % self.kernel)
        except SocketException as se:
            logger.info(se)
            raise WolframKernelException(
                'Failed to communicate with the kernel %s. Could not read from ZMQ socket.'
                % self.kernel)

    def _ensure_started(self):
        if not self.started:
            self.start()
        if self.stopped:
            self.restart()

    @property
    def pid(self):
        """Return the PID of the Wolfram Kernel process, if any, or None."""
        if self.kernel_proc:
            return self.kernel_proc.pid
        else:
            return None

    def _evaluate(self, expr, **kwargs):
        """Send an expression to the kernel for evaluation. Return a :class:`~wolframclient.evaluation.result.WolframKernelEvaluationResult`.
        """
        self._ensure_started()
        assert (self.started)
        start = time.perf_counter()
        if self.wxf_bytes_evaluation and isinstance(expr, six.binary_type):
            data = expr
        else:
            data = export(expr, target_format='wxf', **kwargs)
        self.in_socket.zmq_socket.send(data)
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Expression sent to kernel in %.06fsec',
                         time.perf_counter() - start)
            start = time.perf_counter()

        # read the message as bytes.
        msg_count = self.out_socket.zmq_socket.recv()
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Message count received from kernel after %.06fsec',
                         time.perf_counter() - start)
        try:
            msg_count = int(msg_count)
        except ValueError:
            raise WolframKernelException(
                'Unexpected message count returned by Kernel %s' % msg_count)
        errmsg = []
        for i in range(msg_count):
            json_msg = self.out_socket.zmq_socket.recv_json()
            errmsg.append((json_msg[0], force_text(json_msg[1])))
        wxf_result = self.out_socket.zmq_socket.recv()
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Expression received from kernel after %.06fsec',
                         time.perf_counter() - start)
        self.evaluation_count += 1
        return WolframKernelEvaluationResult(
            wxf_result, errmsg, consumer=self.consumer)

    def evaluate_wxf(self, expr, **kwargs):
        """Send an expression to the kernel for evaluation and return the raw result still encoded as WXF.
        """
        result = self._evaluate(self.normalize_input(expr), **kwargs)
        if not result.success:
            for msg in result.messages:
                logger.warning(msg[1])
        return result.wxf

    def evaluate_wrap(self, expr, **kwargs):
        """ Similar to :func:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate` but return the result as a :class:`~wolframclient.evaluation.result.WolframKernelEvaluationResult`.
        """
        return self._evaluate(self.normalize_input(expr), **kwargs)

    def evaluate(self, expr, **kwargs):
        """Send an expression to the kernel for evaluation.

        The `expr` can be:

            * a text string representing the Wolfram Language expression :wl:`InputForm`.
            * an instance of Python object serializable as WXF by :func:`~wolframclient.serializers.export`.
            * a binary string of a serialized expression in the WXF format.

        `kwargs` are passed to :func:`~wolframclient.serializers.export` during serialization step of
        non-string inputs.
        """
        result = self._evaluate(self.normalize_input(expr), **kwargs)
        if not result.success:
            for msg in result.messages:
                logger.warning(msg[1])
        return result.get()

    def __repr__(self):
        if self.started:
            return '<%s: pid:%i, kernel sockets: (in:%s, out:%s)>' % (
                self.__class__.__name__, self.kernel_proc.pid,
                self.in_socket.uri, self.out_socket.uri)
        else:
            return '<%s: not started>' % self.__class__.__name__


class SocketException(Exception):
    pass


class Socket(object):
    def __init__(self, host='127.0.0.1', port=None, zmq_type=zmq.PAIR):
        self.host = host
        self.port = port
        self._use_random_port = port is None
        if self.port:
            self.uri = 'tcp://%s:%s' % (self.host, self.port)
        else:
            self.uri = 'tcp://' + self.host
        self.zmq_type = zmq_type
        self.bound = False
        self.zmq_socket = zmq.Context.instance().socket(zmq_type)
        self.closed = False

    def bind(self):
        if self.bound:
            raise SocketException('Already bounded.')
        if self.closed:
            raise SocketException('Socket has been closed.')
        self.zmq_socket = zmq.Context.instance().socket(self.zmq_type)
        if self._use_random_port:
            logger.debug('Binding socket using random port.')
            self.port = self.zmq_socket.bind_to_random_port(self.uri)
            self.uri = 'tcp://%s:%s' % (self.host, self.port)
        else:
            self.zmq_socket.bind(self.uri)
        logger.debug('ZMQ socket bound to ' + self.uri)
        self.bound = True
        return self.zmq_socket

    def read_timeout(self,
                     timeout=2.,
                     retry_sleep_time=0.001,
                     sleep=time.sleep):
        """ Read a socket in a non-blocking fashion, until a timeout is reach, retrying at a given interval.
        The sleep function is passed as parameter and can be conveniently modify to support Event based interruption.
        """
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
                try:
                    sleep(retry_sleep_time)
                except TimeoutError:
                    break
        raise SocketException(
            'Read time out. Failed to read any message from socket %s after %.1f seconds and %i retries.'
            % (self.uri, time.perf_counter() - start, retry))

    def close(self):
        self.zmq_socket.close()
        self.closed = True
        self.bound = False

    def __repr__(self):
        if self.bound:
            return '<Socket: host=%s, port=%s>' % (self.host, self.port)
        else:
            return '<Socket (not bounded): host=%s>' % self.host
