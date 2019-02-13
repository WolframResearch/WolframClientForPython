# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
from subprocess import PIPE
from wolframclient.evaluation.base import WolframEvaluator
from wolframclient.evaluation.kernel.kernelcontroller import KernelController
from concurrent import futures

import logging

class WolframLanguageSession(WolframEvaluator):
    """A session to a Wolfram Kernel enabling evaluation of Wolfram Language expressions.

    Start a new session and send an expression for evaluation::

        with WolframLanguageSession() as session:
            session.evaluate('Range[3]')

    Set `timeout` value to a number, to set an evaluation timeout in seconds. If the evaluation
    time extends the timeout a :class:`~concurrent.futures.TimeoutError` is raised.
    
    Evaluate an expression taking 10 seconds to return, using a 5 second timeout::

        long_evaluation = wl.Pause(10)
        with WolframLanguageSession() as session:
            session.evaluate(long_evaluation, timeout=5)

    Asynchronous evaluation methods :meth:`~wolframclient.evaluation.kernel.localsession.WolframLanguageSession.evaluate_future` 
    returns an instance of :class:`~concurrent.futures.Future` class wrapping the evaluation result.

        with WolframLanguageSession() as session:
            future = session.evaluate_future('1+1')
            result = future.result()

    When `consumer` is set to a :class:`~wolframclient.deserializers.WXFConsumer` instance,
    this instance is passed to :func:`~wolframclient.deserializers.binary_deserialize` when
    deserializing the WXF output. 
    
    By default packed arrays are deserialized as :class:`list`. Specify a consumer instance
    that supports NumPy arrays :class:`~wolframclient.deserializers.WXFConsumerNumpy`::

        from wolframclient.deserializers import WXFConsumerNumpy

        with WolframLanguageSession(consumer=WXFConsumerNumpy()) as session:
            numpy_array = session.evaluate('Range[3]')

    Communication with a given kernel is based on ZMQ sockets:

    * one `PUSH` socket to send expressions for evaluation
    * one `PULL` socket to receive evaluation results

    Kernel logging is disabled by default and is done through a third socket
    (type `SUB`). The initial log level is specified by the parameter `kernel_loglevel`.
    If the log level was not set at initialization, logging is not available for the entire
    session.

    The kernel associated with a given session provides the following
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

    The standard input, output and error file handles can be specified with `stdin`, `stdout` and `stderr`
    named parameters. Valid values are those accepted by subprocess.Popen (e.g :data:`sys.stdout`). Those parameters should be handled
    with care as deadlocks can arise from misconfiguration.

    .. note ::
        Wolfram Language sessions are **not thread-safe**, each thread must run its own instance.

    """
    def __init__(self,
                 kernel=None,
                 consumer=None,
                 initfile=None,
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
        self.stopped = False

    @property
    def started(self):
        return self.kernel_controller.started

    def start(self, block=False, timeout=None):
        """ Start a kernel controller, eventually restart a fresh on if the previous one was terminated. 
        
        Set `block` to :data:`True` (default is :data:`False`) to wait for the kernel to be up and running 
        before returning. Eventually set a timeout in seconds. If the timeout is reached a :data:`TimeoutError`
        will be raised.
        """
        self.stopped = False
        try:
            if self.kernel_controller.terminated:
                self.kernel_controller = self.kernel_controller.duplicate()
            if not self.started:
                self.kernel_controller.start()
                if block:
                    started = self.kernel_controller.wait_kernel_ready(timeout=timeout)
                    if not started:
                        raise TimeoutError('Kernel is not started after %.02f seconds' % timeout)
                else:
                    return True
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
        if self.kernel_controller.terminated:
            return
        try:
            if self.kernel_controller.started:
                if gracefully:
                    self.kernel_controller.stop()
                else:
                    self.kernel_controller.terminate()
        finally:
            self.kernel_controller.join()

    def ensure_started(self, block=False, timeout=None):
        if not self.started:
            self.start(block=block, timeout=timeout)
        if self.stopped:
            self.restart()

    def restart(self, block=False, timeout=None):
        """ Restart a given evaluator by stopping it in case it was already started. """
        if self.started:
            self.stop()
        self.start(block=block, timeout=timeout)

    def _evaluate_future(self, expr, future, result_update_callback=None, **kwargs):
        self.ensure_started()
        self.kernel_controller.evaluate_future(expr, future, result_update_callback, **kwargs)

    def evaluate_future(self, expr, **kwargs):
        future = futures.Future()
        self._evaluate_future(self.normalize_input(expr), future, result_update_callback=lambda result : result.get(), **kwargs)
        return future
    
    def evaluate_wxf_future(self, expr, **kwargs):
        future = futures.Future()
        self._evaluate_future(self.normalize_input(expr), future, result_update_callback=lambda result : result.wxf, **kwargs)
        return future

    def evaluate_wrap_future(self, expr, **kwargs):
        future = futures.Future()
        self._evaluate_future(self.normalize_input(expr), future, **kwargs)
        return future

    def _timed_out_eval(self, method, expr, timeout=None, **kwargs):
        if timeout and timeout > 0:
            self.ensure_started()
            expr = wl.TimeConstrained(expr, timeout)
        future = method(expr, **kwargs)
        return future.result(timeout=timeout)

    def evaluate_wrap(self, expr, timeout=None, **kwargs):
        return self._timed_out_eval(self.evaluate_wrap_future, expr, timeout=timeout, **kwargs)

    def evaluate(self, expr, timeout=None, **kwargs):
        return self._timed_out_eval(self.evaluate_future, expr, timeout=timeout, **kwargs)

    def evaluate_wxf(self, expr, timeout=None, **kwargs):
        return self._timed_out_eval(self.evaluate_wxf_future, expr, timeout=timeout, **kwargs)


# class WolframLanguageSession(WolframEvaluator):
#     """A session to a Wolfram Kernel enabling evaluation of Wolfram Language expressions.

#     Start a new session and send an expression for evaluation::

#         with WolframLanguageSession() as session:
#             session.evaluate('Range[3]')

#     Set `timeout` value to a number, to set an evaluation timeout in seconds. If the evaluation
#     time extends the timeout a :class:`~concurrent.futures.TimeoutError` is raised.
    
#     Evaluate an expression taking 10 seconds to return, using a 5 second timeout::

#         long_evaluation = wl.Pause(10)
#         with WolframLanguageSession() as session:
#             session.evaluate(long_evaluation, timeout=5)

#     Asynchronous evaluation methods :meth:`~wolframclient.evaluation.kernel.localsession.WolframLanguageSession.evaluate_future` 
#     returns an instance of :class:`~concurrent.futures.Future` class wrapping the evaluation result.

#         with WolframLanguageSession() as session:
#             future = session.evaluate_future('1+1')
#             result = future.result()

#     When `consumer` is set to a :class:`~wolframclient.deserializers.WXFConsumer` instance,
#     this instance is passed to :func:`~wolframclient.deserializers.binary_deserialize` when
#     deserializing the WXF output. 
    
#     By default packed arrays are deserialized as :class:`list`. Specify a consumer instance
#     that supports NumPy arrays :class:`~wolframclient.deserializers.WXFConsumerNumpy`::

#         from wolframclient.deserializers import WXFConsumerNumpy

#         with WolframLanguageSession(consumer=WXFConsumerNumpy()) as session:
#             numpy_array = session.evaluate('Range[3]')

#     Communication with a given kernel is based on ZMQ sockets:

#     * one `PUSH` socket to send expressions for evaluation
#     * one `PULL` socket to receive evaluation results

#     Kernel logging is disabled by default and is done through a third socket
#     (type `SUB`). The initial log level is specified by the parameter `kernel_loglevel`.
#     If the log level was not set at initialization, logging is not available for the entire
#     session.

#     The kernel associated with a given session provides the following
#     logging functions:

#     * ``ClientLibrary`debug`` corresponding to :py:meth:`logging.Logger.debug`
#     * ``ClientLibrary`info`` corresponding to :py:meth:`logging.Logger.info`
#     * ``ClientLibrary`warn`` corresponding to :py:meth:`logging.Logger.warning`
#     * ``ClientLibrary`error`` corresponding to :py:meth:`logging.Logger.error`
#     * ``ClientLibrary`SetDebugLogLevel[]`` send debug messages and above
#     * ``ClientLibrary`SetInfoLogLevel[]`` send info messages and above
#     * ``ClientLibrary`SetWarnLogLevel[]`` send warning messages and above
#     * ``ClientLibrary`SetErrorLogLevel[]`` only send error messages
#     * ``ClientLibrary`DisableKernelLogging[]`` stop sending error message to the logging socket

#     The standard input, output and error file handles can be specified with `stdin`, `stdout` and `stderr`
#     named parameters. Valid values are those accepted by subprocess.Popen (e.g :data:`sys.stdout`). Those parameters should be handled
#     with care as deadlocks can arise from misconfiguration.

#     .. note ::
#         Wolfram Language sessions are **not thread-safe**, each thread must run its own instance.

#     """
#     def __init__(self,
#                  kernel=None,
#                  consumer=None,
#                  initfile=None,
#                  in_socket=None,
#                  out_socket=None,
#                  kernel_loglevel=logging.NOTSET,
#                  stdin=PIPE,
#                  stdout=PIPE,
#                  stderr=PIPE,
#                  inputform_string_evaluation=True,
#                  wxf_bytes_evaluation=True,
#                  **kwargs):
#         super().__init__(
#             inputform_string_evaluation=inputform_string_evaluation)
#         self.wxf_bytes_evaluation=wxf_bytes_evaluation
#         self.thread_pool_exec = None
#         self.kernel_evaluator = KernelEvaluator(
#             kernel=kernel,
#             consumer=consumer,
#             initfile=initfile,
#             kernel_loglevel=kernel_loglevel,
#             stdin=stdin,
#             stdout=stdout,
#             stderr=stderr,
#             **kwargs)

#     def evaluate(self, expr, timeout=None, **kwargs):
#         return self.eval_timeout(super().evaluate, expr, timeout=timeout, **kwargs)

#     def evaluate_wxf(self, expr, timeout=None, **kwargs):
#         return self.eval_timeout(super().evaluate_wxf, expr, timeout=timeout, **kwargs)

#     def evaluate_wrap(self, expr, timeout=None, **kwargs):
#         return self.eval_timeout(super().evaluate_wrap, expr, timeout=timeout, **kwargs)
    
#     def eval_timeout(self, func, expr, timeout=None, **kwargs):
#         # make sure the session is started otherwise the timeout is also applied to initialization 
#         # which is pretty long (around a second).
#         expr = self.normalize_input(expr)
#         if timeout and timeout > 0:
#             super().ensure_started()
#             expr = wl.TimeConstrained(expr, timeout)
#         future = self._do_in_thread(func, expr, **kwargs)
#         return future.result(timeout=timeout)

#     def evaluate_future(self, expr, **kwargs):
#         return self._do_in_thread(super().evaluate, normalize_input(expr), **kwargs)
    
#     def evaluate_wxf_future(self, expr, **kwargs):
#         return self._do_in_thread(super().evaluate_wxf, normalize_input(expr), **kwargs)

#     def evaluate_wrap_future(self, expr, **kwargs):
#         return self._do_in_thread(super().evaluate_wrap, normalize_input(expr), **kwargs)

#     def start(self, timeout=None):
#         return self._do_in_thread(super().start).result(timeout=timeout)

#     def start_future(self, timeout=None):
#         return self._do_in_thread(super().start)

#     def stop(self, timeout=None):
#         return self._do_in_thread(super().stop).result(timeout=timeout)

#     def stop_future(self, timeout=None):
#         return self._do_in_thread(super().stop)

#     def _do_in_thread(self, func, *args, **kwargs):
#         try:
#             self._get_exec_pool()
#             return self.thread_pool_exec.submit(func, *args, **kwargs)
#         except ImportError:
#             logger.fatal('Module concurrent.futures is missing.')
#             raise NotImplementedError(
#                 'Asynchronous evaluation is not available on this Python interpreter.'
#             )

#     def _get_exec_pool(self):
#         if self.thread_pool_exec is None:
#             self.thread_pool_exec = futures.ThreadPoolExecutor(max_workers=1)
#         return self.thread_pool_exec

#     def terminate(self):
#         """Terminate the current session. This is a synchronous method."""
#         # First terminate all executions.
#         # Then use the socket to actually quit. Avoid crashes when freeing zmq resources still in use.
#         try:
#             if self.thread_pool_exec:
#                 self.thread_pool_exec.shutdown(wait=True)
#         except Exception as e:
#             logger.fatal('Failed to stop thread pool executor: %s' % e)
#             raise e
#         finally:
#             super().terminate()
