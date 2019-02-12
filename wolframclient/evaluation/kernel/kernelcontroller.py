# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from threading import Event, Thread
from subprocess import PIPE, Popen
import logging

from wolframclient.utils import six
from wolframclient.utils.api import json, os, time, zmq
from wolframclient.exception import WolframKernelException
from wolframclient.serializers import export
from wolframclient.evaluation.kernel.zmqsocket import Socket, SocketException
from wolframclient.evaluation.result import WolframKernelEvaluationResult
from wolframclient.evaluation.base import WolframEvaluator
if six.WINDOWS:
    from subprocess import STARTUPINFO, STARTF_USESHOWWINDOW

logger = logging.getLogger(__name__)

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
        Thread.__init__()
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

if six.WINDOWS or six.LINUX:
    if six.WINDOWS:
        APP_ROOT_PATH = [
            'C:\\Program Files\\Wolfram Research\\Wolfram Desktop\\',
            'C:\\Program Files\\Wolfram Research\\Mathematica\\',
            ]
        EXE_REL_PATH = 'wolfram.exe'
    elif six.LINUX:
        APP_ROOT_PATH = [
            '/usr/local/Wolfram/Desktop/',
            '/usr/local/Wolfram/Mathematica/',
            ]
        EXE_REL_PATH = '/Files/Executables/wolfram'

    def find_default_kernel_path():
        highest_version = -1
        best_path = None
        for root in APP_ROOT_PATH:
            if os.isdir(root):
                for version in os.listdir(root):
                    full_path = os.path_join(root, version)
                    if os.isdir(full_path):
                        try:
                            v_num = float(version)
                        except ValueError:
                            continue
                        if v_num > highest_version:
                            highest_version = v_num
                            best_path = full_path
        if highest_version > 0:
            return os.path_join(best_path, EXE_REL_PATH)
        else:
            return None
elif six.MACOS:
    DEFAULT_PATHS = [
            '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel',
            '/Applications/Mathematica.app/Contents/MacOS/WolframKernel',
        ]
    def find_default_kernel_path():
        for path in DEFAULT_PATHS:
            if os.isfile(path):
                return path
        return None
else:
    def find_default_kernel_path():
        return None

find_default_kernel_path.__doc__ = """ Look for the most recent installed kernel. """



class KernelController(Thread):
    
    def __init__(self,
                 kernel=None,
                 initfile=None,
                 kernel_loglevel=logging.NOTSET,
                 stdin=PIPE,
                 stdout=PIPE, 
                 stderr=PIPE, 
                 **kwargs):
        super().__init__()
        if kernel is None:
            kernel = self.default_kernel_path()
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
        self.inproc_socket_in_uri = None
        self.inproc_socket_out_uri = None

        self.kernel_socket_in = None
        self.kernel_socket_out = None
        self.kernel_proc = None
        self.loglevel = kernel_loglevel
        self.kernel_logger = None
        self.evaluation_count = 0
        self._stdin = stdin
        self._stdout = stdout
        self._stderr = stderr
        self.stop_event = Event()
        self.terminate_event = Event()
        # some parameters may be passed as kwargs
        self.parameters = {}
        for k, v in kwargs.items():
            self.set_parameter(k, v)

    def duplicate(self):
        """ Build a new object using the same configuration as the current one. """
        return self.__class__(
            kernel=self.kernel,
            initfile=self.initfile,
            kernel_loglevel=self.loglevel,
            stdin=self._stdin,
            stdout=self._stdout,
            stderr=self._stderr
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
        * ``'STARTUP_RETRY_SLEEP_TIME'``: time to sleep, in seconds, before checking that the initialized kernel has responded. Default is 1 ms.
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
        * ``'STARTUP_RETRY_SLEEP_TIME'``: time to sleep, in seconds, before checking that the initialized kernel has responded. Default is 1 ms.
        * ``'TERMINATE_READ_TIMEOUT'``: time to wait, in seconds, after the ``Quit[]`` command was sent to the kernel. The kernel is killed after this duration. Default is 3 seconds.
        """
        if parameter_name not in self._DEFAULT_PARAMETERS:
            raise KeyError(
                '%s is not one of the valid parameters: %s' %
                (parameter_name, ', '.join(self._DEFAULT_PARAMETERS.keys())))
        self.parameters[parameter_name] = parameter_value
    
    def default_kernel_path(self):
        return find_default_kernel_path()
    
    def connect(self):
        """ Return a pair of bound sockets to use to send expression, and
        receive result.

        Return a pair of a PUSH socket to write to, and a PULL socket to read from, as a tuple.
        Once this method has been called, it save the target URIs, and will always use them.
        """
        send_expr_socket = Socket(zmq_type=zmq.PUSH)
        read_result_socket = Socket(zmq_type=zmq.PULL)
        if self.inproc_socket_in_uri:
            send_expr_socket.bind_to_uri(self.inproc_socket_in_uri)
        else:
            send_expr_socket.bind(protocol='inproc', host='wolfctrlr-in-%i' % hash(self))
            self.inproc_socket_in_uri = send_expr_socket.uri
        if self.inproc_socket_out_uri:
            read_result_socket.bind_to_uri(self.inproc_socket_out_uri)
        else:
            read_result_socket.bind(protocol='inproc', host='wolfctrlr-out-%i' % hash(self))
            self.inproc_socket_out_uri = read_result_socket.uri
        return (send_expr_socket, read_result_socket)

    def close(self):
        """Close the kernel controller connections.
        
        This is different from joining the thread. Release inproc sockets. """
        try:
            self.close_inproc_socket_in()
        finally:
            self.close_inproc_socket_out()

    def close_inproc_socket_in(self):
        try:
            if self.inproc_socket_in:
                self.inproc_socket_in.close()
        finally:
            self.inproc_socket_in = None
            self.inproc_socket_in_uri = None
    
    def close_inproc_socket_out(self):
        try:
            if self.inproc_socket_out:
                self.inproc_socket_out.close()
        finally:
            self.inproc_socket_out = None
            self.inproc_socket_out_uri = None

    def _kernel_terminate(self):
        self._kernel_stop(gracefully=False)

    def _kernel_stop(self, gracefully=True):
        """Stop the kernel process and close sockets.

        This function must be called when a given session is no longer useful
        to prevent orphan processes and sockets from being generated.

        .. note::
            Licensing restrictions usually apply to Wolfram kernels and may
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
                    self.kernel_socket_out.send(
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
                # Wait for it to cleanly stop if the Quit command was successfully sent,
                # otherwise the kernel is likely in a bad state so we kill it immediately.
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
        if self.kernel_socket_out is not None:
            try:
                self.kernel_socket_out.close()
            except Exception as e:
                logger.fatal(e)
            finally:
                self.kernel_socket_out = None
        if self.kernel_socket_in is not None:
            try:
                self.kernel_socket_in.close()
            except Exception as e:
                logger.fatal(e)
            finally:
                self.kernel_socket_in = None
        if self.kernel_logger is not None:
            try:
                self.kernel_logger.stopped.set()
                self.kernel_logger.join()
            except Exception as e:
                logger.fatal(e)
            finally:
                self.kernel_logger = None
        assert (self.kernel_proc is None)
        assert (self.kernel_socket_in is None)
        assert (self.kernel_socket_out is None)
        assert (self.kernel_logger is None)

    @property
    def kernel_started(self):
        return (self.kernel_proc is not None and self.kernel_proc.poll() is None
                and self.kernel_socket_out and self.kernel_socket_out.bound 
                and self.kernel_socket_in and self.kernel_socket_in.bound)

    def kernel_start(self):
        """ Start a kernel. If something went wrong, clean-up resources that may have been created. """
        try:
            self._kernel_start()
        except Exception as e:
            try:
                self._kernel_terminate()
            finally:
                assert (self.stopped)
                assert (not self.kernel_started)
                raise e

    _KERNEL_OK = b'OK'

    def _kernel_start(self):
        """Start a new kernel process and open sockets to communicate with it."""
        self.stopped = False
        if self.kernel_started:
            return
        # Socket to which we push new expressions for evaluation.
        if self.kernel_socket_out is None:
            self.kernel_socket_out = Socket(zmq_type=zmq.PUSH)
        if self.kernel_socket_in is None:
            self.kernel_socket_in = Socket(zmq_type=zmq.PULL)
        # start the evaluation zmq sockets
        self.kernel_socket_out.bind()
        self.kernel_socket_in.bind()
        if logger.isEnabledFor(logging.INFO):
            logger.info('Kernel writes commands from socket: %s',
                        self.kernel_socket_out)
            logger.info('Kernel receives evaluated expressions to socket: %s',
                        self.kernel_socket_in)
        # start the kernel process
        cmd = [self.kernel, '-noprompt', "-initfile", self.initfile]
        if self.loglevel != logging.NOTSET:
            self.kernel_logger = KernelLogger(level=self.loglevel)
            self.kernel_logger.start()
            cmd.append('-run')
            cmd.append(
                'ClientLibrary`Private`SlaveKernelPrivateStart["%s", "%s", "%s", %i];'
                % (self.kernel_socket_out.uri, self.kernel_socket_in.uri,
                   self.kernel_logger.socket.uri,
                   FROM_PY_LOG_LEVEL[self.loglevel]))
        else:
            cmd.append('-run')
            cmd.append(
                'ClientLibrary`Private`SlaveKernelPrivateStart["%s", "%s"];' %
                (self.kernel_socket_out.uri, self.kernel_socket_in.uri))

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
            response = self.kernel_socket_in.recv_timeout(
                timeout=self.get_parameter('STARTUP_READ_TIMEOUT'),
                retry_sleep_time=self.get_parameter('STARTUP_RETRY_SLEEP_TIME')
            )
            if response == self._KERNEL_OK:
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
    @property
    def pid(self):
        """Return the PID of the Wolfram Kernel process, if any, or None."""
        if self.kernel_proc:
            return self.kernel_proc.pid
        else:
            return None
    
    def run(self):
        graceful_stop = None
        if self.inproc_socket_in_uri is None or self.inproc_socket_out_uri is None:
            raise WolframKernelException('Controller is not connected.')
        try:
            self.inproc_socket_in = Socket(zmq_type=zmq.PULL)
            self.inproc_socket_out = Socket(zmq_type=zmq.PUSH)
            logger.debug('PUSH inproc: %s, PULL kernel: %s', self.inproc_socket_out, self.inproc_socket_in_uri)
            self.inproc_socket_in.zmq_socket.connect(self.inproc_socket_in_uri)
            self.inproc_socket_out.zmq_socket.connect(self.inproc_socket_out_uri)

            # wait for the command to initialize the kernel
            while not self.stop_event.is_set() and not self.terminate_event.is_set():
                if self.inproc_socket_in.poll(timeout=.1) > 0:
                    self.kernel_start()
                    break
            while True:
                if self.stop_event.is_set():
                    graceful_stop = True
                    break
                if self.terminate_event.is_set():
                    graceful_stop = False
                    break
                if self.inproc_socket_in.poll(timeout=.1) > 0:
                    wxf_frame = self.inproc_socket_in.recv(copy=False)
                    start = time.perf_counter()
                    self.kernel_socket_out.send(wxf_frame)
                    if logger.isEnabledFor(logging.DEBUG):
                        logger.debug('Expression sent to kernel in %.06fsec',
                                    time.perf_counter() - start)
                        start = time.perf_counter()

                    # read the message as bytes.
                    msg_count = self.kernel_socket_in.recv()
                    if logger.isEnabledFor(logging.DEBUG):
                        logger.debug('Message count received from kernel after %.06fsec',
                                    time.perf_counter() - start)
                    try:
                        msg_count_as_int = int(msg_count)
                    except ValueError:
                        raise WolframKernelException(
                            'Unexpected message count returned by Kernel %s' % msg_count)
                    # self.inproc_socket_out.send(msg_count)
                    # TODO use EvaluationData to get one message with result and metadata.
                    errmsg = []
                    for i in range(msg_count_as_int):
                        json_msg = self.kernel_socket_in.recv_json()
                        errmsg.append((json_msg[0], force_text(json_msg[1])))
                        logger.warn(json_msg)
                    
                    wxf_result = self.kernel_socket_in.recv(copy=False)
                    if logger.isEnabledFor(logging.DEBUG):
                        logger.debug('Expression received from kernel after %.06fsec',
                                    time.perf_counter() - start)
                    self.evaluation_count += 1
                    self.inproc_socket_out.send(wxf_result)
        finally:
            try:
                if graceful_stop:
                    self._kernel_stop()
                else:
                    self._kernel_terminate()
            finally:
                self.close()

class KernelEvaluator(WolframEvaluator):
    def __init__(self,
                 kernel=None,
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
        self.wxf_bytes_evaluation=wxf_bytes_evaluation
        self.kernel_controller = KernelController(
            kernel=kernel,
            initfile=initfile,
            kernel_loglevel=kernel_loglevel,
            stdin=stdin,
            stdout=stdout,
            stderr=stderr,
            **kwargs)
        self.inproc_socket_out = None
        self.inproc_socket_in = None
        self.consumer = consumer
        self.stopped = False

    @property
    def started(self):
        return self.kernel_controller.is_alive()

    _socket_read_sleep_func = time.sleep

    def start(self):
        self.stopped = False
        try:
            self.inproc_socket_out, self.inproc_socket_in = self.kernel_controller.connect()
            # start the thread.
            self.kernel_controller.start() 
        except Exception as e:
            try:
                self.terminate()
            finally:
                raise e

    def stop(self):
        self._stop(gracefully=True)
    
    def terminate(self):
        self._stop(gracefully=False)

    def _stop(self, gracefully=True):
        self.stopped = True
        if self.kernel_controller.is_alive():
            try:
                if gracefully:
                    self.kernel_controller.stop_event.set()
                else:
                    self.kernel_controller.terminate_event.set()
            finally:
                self.kernel_controller.join()

    def ensure_started(self):
        if not self.started:
            self.start()
        if self.stopped:
            self.restart()

    def do_evaluate(self, expr, **kwargs):
        if not self.started:
            raise WolframKernelException('Kernel is not running.')
        wxf = export(expr, target_format='wxf', **kwargs)
        self.inproc_socket_out.send(wxf, copy=False)
        wxf_frame = self.inproc_socket_in.recv(copy=False)
        return WolframKernelEvaluationResult(wxf_frame.buffer, [], consumer=self.consumer)

    def evaluate_wrap(self, expr, **kwargs):
        expr = self.normalize_input(expr)
        return self.do_evaluate(expr, **kwargs)