# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
from concurrent import futures
from itertools import count as _count
from queue import Queue
from subprocess import PIPE, Popen
from threading import Event, RLock, Thread

from wolframclient.evaluation.kernel.path import find_default_kernel_path
from wolframclient.evaluation.kernel.zmqsocket import (Socket, SocketAborted,
                                                       SocketOperationTimeout)
from wolframclient.evaluation.result import WolframKernelEvaluationResult
from wolframclient.exception import WolframKernelException
from wolframclient.utils import six
from wolframclient.utils.api import json, os, time, zmq

if six.WINDOWS:
    from subprocess import STARTUPINFO, STARTF_USESHOWWINDOW

__all__ = ['WolframKernelController']

logger = logging.getLogger(__name__)

TO_PY_LOG_LEVEL = {
    1: logging.DEBUG,
    2: logging.INFO,
    3: logging.WARN,
    4: logging.FATAL
}
FROM_PY_LOG_LEVEL = dict((v, k) for k, v in TO_PY_LOG_LEVEL.items())

_thread_counter = _count().__next__
_thread_counter()


class KernelLogger(Thread):
    """ Asynchronous logger for kernel messages. 
    
    A consumer of messages read from a PUB/SUB socket that turn them into log messages as expected
    by the :mod:`logging` module.
    """
    MAX_MESSAGE_BEFORE_QUIT = 32

    def __init__(self, name=None, level=logging.WARN):
        super().__init__(name=name)
        self.socket = Socket(zmq_type=zmq.SUB)
        self.socket.bind()
        # Subscribe to all since we want all log messages.
        self.socket.zmq_socket.setsockopt(zmq.SUBSCRIBE, b'')
        if logger.isEnabledFor(logging.INFO):
            logger.info('Initializing Kernel logger on socket ' +
                        self.socket.uri)
        self.logger = logging.getLogger('WolframKernel-<%s>' % self.socket.uri)
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


class WolframKernelController(Thread):
    """ Control a Wolfram kernel from a Python thread.

    A controller can start and stop a Wolfram kernel specified by its path `kernel`. It
    can evaluate expression, one at a time.

    Most methods from this class return instances of :class:`~concurrent.futures.Future`.

    This class is a low level component of the library which is used by local evaluators.

    ZMQ sockets are not thread safe, this class ensures encapsulation of them, while enabling
    asynchronous operations.
    """

    def __init__(self,
                 kernel=None,
                 initfile=None,
                 consumer=None,
                 kernel_loglevel=logging.NOTSET,
                 stdin=PIPE,
                 stdout=PIPE,
                 stderr=PIPE,
                 **kwargs):
        self.id = _thread_counter()
        super().__init__(name='wolfram-kernel-%i' % self.id)
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
        self.tasks_queue = Queue()
        self.kernel_socket_in = None
        self.kernel_socket_out = None
        self.kernel_proc = None
        self.consumer = consumer
        self.loglevel = kernel_loglevel
        self.kernel_logger = None
        self.evaluation_count = 0
        self._stdin = stdin
        self._stdout = stdout
        self._stderr = stderr
        # some parameters may be passed as kwargs
        self.parameters = {}
        for k, v in kwargs.items():
            try:
                self.set_parameter(k, v)
            # ignore kwargs unknowns key
            except KeyError:
                pass
        # this is a state: this event is set when the kernel will not serve any more evaluation.
        self._state_terminated = False
        # lock controlling concurrent access to the state above.
        self._state_lock = RLock()
        # this is a trigger that will abort most blocking operations.
        self.trigger_termination_requested = Event()

    def duplicate(self):
        """ Build a new object using the same configuration as the current one. """
        return self.__class__(
            kernel=self.kernel,
            initfile=self.initfile,
            kernel_loglevel=self.loglevel,
            consumer=self.consumer,
            stdin=self._stdin,
            stdout=self._stdout,
            stderr=self._stderr,
            **self.parameters)

    _DEFAULT_PARAMETERS = {
        'STARTUP_TIMEOUT': 20,
        'TERMINATE_TIMEOUT': 3,
        'HIDE_SUBPROCESS_WINDOW': True
    }

    def get_parameter(self, parameter_name):
        """Return the value of a given session parameter.

        Session parameters are:

        * ``'STARTUP_TIMEOUT'``: time to wait, in seconds, after the kernel startup is requested. Default is 20 seconds.
        * ``'TERMINATE_TIMEOUT'``: time to wait, in seconds, after the ``Quit[]`` command is sent to the kernel. The kernel is killed after this duration. Default is 3 seconds.
        """
        try:
            return self.parameters.get(
                parameter_name, self._DEFAULT_PARAMETERS[parameter_name])
        except KeyError:
            raise KeyError(
                '%s is not one of the valid parameters: %s' %
                (parameter_name, ', '.join(self._DEFAULT_PARAMETERS.keys())))

    def set_parameter(self, parameter_name, parameter_value):
        """Set a new value for a given parameter. The new value only applies for this session.

        Session parameters are:

        * ``'STARTUP_TIMEOUT'``: time to wait, in seconds, after the kernel startup is requested. Default is 20 seconds.
        * ``'TERMINATE_TIMEOUT'``: time to wait, in seconds, after the ``Quit[]`` command is sent to the kernel. The kernel is killed after this duration. Default is 3 seconds.
        """
        if parameter_name not in self._DEFAULT_PARAMETERS:
            raise KeyError(
                '%s is not one of the valid parameters: %s' %
                (parameter_name, ', '.join(self._DEFAULT_PARAMETERS.keys())))
        self.parameters[parameter_name] = parameter_value

    def default_kernel_path(self):
        return find_default_kernel_path()

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
        with self._state_lock:
            self._state_terminated = True
        self.trigger_termination_requested.set()
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
                            timeout=self.get_parameter('TERMINATE_TIMEOUT'))
                    except:
                        logger.info(
                            'Kernel process failed to stop after %.02f seconds. Killing it.'
                            % self.get_parameter('TERMINATE_TIMEOUT'))
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
    def started(self):
        """ Is the kernel starting or being started. """
        with self._state_lock:
            return self.is_alive() and not self._state_terminated

    @property
    def terminated(self):
        """ Is the kernel terminated. Terminated kernel no more handle evaluations. """
        with self._state_lock:
            return self._state_terminated

    def is_kernel_alive(self):
        """ Return the status of the kernel process. """
        try:
            # subprocess poll function is thread safe.
            return (self.kernel_proc is not None
                    and self.kernel_proc.poll() is None)
        except AttributeError:
            # in case kernel_proc was set to None. May not even be possible.
            return False

    def request_kernel_start(self):
        """ Start the thread and the associated kernel. Return a future object indicating the kernel status.
        
        The future object result is True once the kernel is successfully started. Exception raised in the process
        and passed to the future object. 
        
        Calling this method twice is a no-op."""
        with self._state_lock:
            future = futures.Future()
            if not self.started:
                self.enqueue_task(self.START, future, None)
                self.start()
            else:
                future.set_result(True)
            return future

    def enqueue_task(self, payload, future, callback):
        if self.terminated or self.trigger_termination_requested.is_set():
            logger.fatal('Cannot enqueue tasks on terminated controller.')
            raise RuntimeError('Thread is closing. Cannot queue task')
        self.tasks_queue.put((payload, future, callback))

    def _safe_kernel_start(self):
        """ Start a kernel. If something went wrong, clean-up resources that may have been created. """
        try:
            self._kernel_start()
        except Exception as e:
            logger.warning('Failed to start.')
            try:
                self._kernel_terminate()
            finally:
                raise e

    _KERNEL_OK = b'OK'
    _KERNEL_VERSION_NOT_SUPPORTED = 10

    def _kernel_start(self):
        """Start a new kernel process and open sockets to communicate with it."""
        # Socket to which we push new expressions for evaluation.
        if self.kernel_socket_out is None:
            self.kernel_socket_out = Socket(zmq_type=zmq.PUSH)
        if self.kernel_socket_in is None:
            self.kernel_socket_in = Socket(zmq_type=zmq.PULL)
        # start the evaluation zmq sockets
        self.kernel_socket_out.bind()
        self.kernel_socket_in.bind()
        if logger.isEnabledFor(logging.INFO):
            logger.info('Kernel writes commands to socket: %s',
                        self.kernel_socket_out)
            logger.info(
                'Kernel receives evaluated expressions from socket: %s',
                self.kernel_socket_in)
        # start the kernel process
        cmd = [self.kernel, '-noprompt', "-initfile", self.initfile]
        if self.loglevel != logging.NOTSET:
            self.kernel_logger = KernelLogger(
                name='wolfram-kernel-logger-%i' % self.id, level=self.loglevel)
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
            response = self.kernel_socket_in.recv_abortable(
                timeout=self.get_parameter('STARTUP_TIMEOUT'),
                abort_event=_StartEvent(self.kernel_proc,
                                        self.trigger_termination_requested))
            if response == self._KERNEL_OK:
                if logger.isEnabledFor(logging.INFO):
                    logger.info(
                        'Kernel %s is ready. Startup took %.2f seconds.' %
                        (self.pid, time.perf_counter() - t_start))
            else:
                raise WolframKernelException(
                    'Kernel %s failed to start properly.' % self.kernel)
        except (SocketAborted, SocketOperationTimeout) as se:
            if self.kernel_proc.returncode == self._KERNEL_VERSION_NOT_SUPPORTED:
                raise WolframKernelException(
                    'Wolfram kernel version is not supported. Please consult library prerequisites.'
                )
            logger.warning('Socket exception: %s', se)
            raise WolframKernelException(
                'Failed to communicate with kernel: %s.' % self.kernel)

    @property
    def pid(self):
        """Return the PID of the Wolfram kernel process, if any, or None."""
        try:
            return self.kernel_proc.pid
        except AttributeError:
            return None

    START = object()
    STOP = object()

    def stop(self):
        future = futures.Future()
        with self._state_lock:
            if self.terminated:
                future.set_result(True)
                return future
            self.enqueue_task(self.STOP, future, None)
            self._state_terminated = True
        return future

    def terminate(self):
        future = futures.Future()
        with self._state_lock:
            if not self.started:
                future.set_result(True)
                return future
            self.enqueue_task(self.STOP, future, None)
            self._state_terminated = True
            self.trigger_termination_requested.set()
        return future

    def evaluate_future(self,
                        wxf,
                        future,
                        result_update_callback=None,
                        **kwargs):
        self.enqueue_task(wxf, future, result_update_callback)

    def _do_evaluate(self, wxf, future, result_update_callback):
        start = time.perf_counter()
        self.kernel_socket_out.send(wxf)
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Expression sent to kernel in %.06fsec',
                         time.perf_counter() - start)
            start = time.perf_counter()
        wxf_eval_data = self.kernel_socket_in.recv_abortable(copy=False)
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Expression received from kernel after %.06fsec',
                         time.perf_counter() - start)
        self.evaluation_count += 1
        result = WolframKernelEvaluationResult(
            wxf_eval_data.buffer, consumer=self.consumer)
        if logger.isEnabledFor(logging.WARNING):
            for msg in result.iter_messages():
                logger.warning(msg)
        if result_update_callback:
            result = result_update_callback(result)
        future.set_result(result)

    def run(self):
        future = None
        task = None
        try:
            task = self.tasks_queue.get()
            payload, future, result_update_callback = task
            # Kernel start requested.
            if payload is self.START:
                self._safe_kernel_start()
                future.set_result(True)
                future = None
                task = None
                self.tasks_queue.task_done()
                task = self.tasks_queue.get()
                payload, future, result_update_callback = task
            elif payload is self.STOP:
                future.set_result(True)
                future = None
                return
            # first evaluation. Ensure kernel is started.
            else:
                self._safe_kernel_start()
            while not self.trigger_termination_requested.is_set():
                if payload is self.STOP:
                    # lock controlling concurrent access to state above.
                    with self._state_lock:
                        self._state_terminated = True
                    logger.info(
                        'Termination requested for kernel controller. Associated kernel PID: %s',
                        self.pid)
                    task = None
                    self.tasks_queue.task_done()
                    break
                self._do_evaluate(payload, future, result_update_callback)
                future = None
                task = None
                self.tasks_queue.task_done()
                task = self.tasks_queue.get()
                payload, future, result_update_callback = task
        except (KeyboardInterrupt, RuntimeError, futures.CancelledError) as e:
            self.trigger_termination_requested.set()
            logger.error('Fatal error in kernel controller: %s', e)
            raise e
        except Exception as e:
            self.trigger_termination_requested.set()
            if future and not future.cancelled():
                future.set_exception(e)
                future = None
            else:
                raise e
        finally:
            try:
                if task:
                    self.tasks_queue.task_done()
                self._cancel_tasks()
                if self.trigger_termination_requested.is_set():
                    self._kernel_terminate()
                else:
                    self._kernel_stop()
            except Exception as e:
                if future:
                    future.set_exception(e)
                    future = None
            finally:
                if future:
                    future.set_result(True)

    def _cancel_tasks(self):
        while not self.tasks_queue.empty():
            task = self.tasks_queue.get()
            _, future, _ = task
            future.cancel()

    def __repr__(self):
        if self.started:
            return '<%s: pid:%i, kernel sockets: (in:%s, out:%s)>' % (
                self.__class__.__name__, self.kernel_proc.pid,
                self.kernel_socket_in.uri, self.kernel_socket_out.uri)
        else:
            return '<%s: %s>' % (self.__class__.__name__, self.name)


class _StartEvent(object):
    def __init__(self, subprocess, abort_event):
        self.subprocess = subprocess
        self.abort_event = abort_event

    def is_set(self):
        return self.subprocess.poll() is not None or self.abort_event.is_set()
