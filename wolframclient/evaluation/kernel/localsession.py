# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
from concurrent import futures
from subprocess import PIPE

from wolframclient.evaluation.base import WolframEvaluator
from wolframclient.evaluation.kernel.kernelcontroller import (
    WolframKernelController)
from wolframclient.serializers import export

logger = logging.getLogger(__name__)

__all__ = ['WolframLanguageSession']


# Some callback methods for internal use.
def do_get_wxf(result):
    return result.wxf


def do_get_result(result):
    return result.get()


class WolframLanguageSession(WolframEvaluator):
    """A session to a Wolfram kernel enabling evaluation of Wolfram Language expressions.

    Start a new session and send an expression for evaluation::

        with WolframLanguageSession() as session:
            session.evaluate('Range[3]')

    Set `timeout` to a number to set an evaluation timeout in seconds. If the evaluation
    time extends the timeout, a :class:`~concurrent.futures.TimeoutError` is raised.
    
    Evaluate an expression taking 10 seconds to return using a 5-second timeout::

        long_evaluation = wl.Pause(10)
        with WolframLanguageSession() as session:
            session.evaluate(long_evaluation, timeout=5)

    The asynchronous evaluation method
    :meth:`~wolframclient.evaluation.kernel.localsession.WolframLanguageSession.evaluate_future`
    returns an instance of :class:`~concurrent.futures.Future` class wrapping the evaluation result::

        with WolframLanguageSession() as session:
            future = session.evaluate_future('1+1')
            result = future.result()

    When `consumer` is set to a :class:`~wolframclient.deserializers.WXFConsumer` instance, this instance is passed to
    :func:`~wolframclient.deserializers.binary_deserialize` when deserializing the WXF output.
    
    By default, packed arrays are deserialized as :class:`list`. Specify a consumer instance that supports NumPy arrays
    :class:`~wolframclient.deserializers.WXFConsumerNumpy`::

        from wolframclient.deserializers import WXFConsumerNumpy

        with WolframLanguageSession(consumer=WXFConsumerNumpy()) as session:
            numpy_array = session.evaluate('Range[3]')

    Communication with a given kernel is based on ZMQ sockets:

    * one `PUSH` socket to send expressions for evaluation
    * one `PULL` socket to receive evaluation results

    Kernel logging is disabled by default and is done through a third socket (type `SUB`). The initial log level is
    specified by the parameter `kernel_loglevel`.
    If the log level was not set at initialization, logging is not available for the entire session.

    The kernel associated with a given session provides the following logging functions:

    * ``ClientLibrary`debug`` corresponding to :py:meth:`logging.Logger.debug`
    * ``ClientLibrary`info`` corresponding to :py:meth:`logging.Logger.info`
    * ``ClientLibrary`warn`` corresponding to :py:meth:`logging.Logger.warning`
    * ``ClientLibrary`error`` corresponding to :py:meth:`logging.Logger.error`
    * ``ClientLibrary`SetDebugLogLevel[]`` send debug messages and above
    * ``ClientLibrary`SetInfoLogLevel[]`` send info messages and above
    * ``ClientLibrary`SetWarnLogLevel[]`` send warning messages and above
    * ``ClientLibrary`SetErrorLogLevel[]`` only send error messages
    * ``ClientLibrary`DisableKernelLogging[]`` stop sending error message to the logging socket

    The standard input, output and error file handles can be specified with `stdin`, `stdout` and `stderr` named
    parameters. Valid values are those accepted by :class:`subprocess.Popen` (e.g. :data:`sys.stdout`). Those parameters
    should be handled with care as deadlocks can arise from misconfiguration.

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
                 controller_class=WolframKernelController,
                 **kwargs):
        super().__init__(
            inputform_string_evaluation=inputform_string_evaluation)
        self.kernel = kernel
        self.consumer = None
        self.initfile = None
        self.kernel_loglevel = logging.NOTSET
        self._stdin = stdin
        self._stdout = stdout
        self._stderr = stderr
        self.wxf_bytes_evaluation = wxf_bytes_evaluation
        self.controller_class = controller_class
        self.kernel_controller = self.controller_class(
            kernel=kernel,
            initfile=initfile,
            kernel_loglevel=kernel_loglevel,
            stdin=stdin,
            stdout=stdout,
            stderr=stderr,
            **kwargs)
        self.parameters = kwargs
        self.stopped = True

    def duplicate(self):
        return self.__class__(
            kernel=self.kernel,
            consumer=self.consumer,
            initfile=self.initfile,
            kernel_loglevel=self.kernel_loglevel,
            stdin=self._stdin,
            stdout=self._stdout,
            stderr=self._stderr,
            inputform_string_evaluation=self.inputform_string_evaluation,
            controller_class=self.controller_class,
            **self.parameters)

    @property
    def started(self):
        return self.kernel_controller.started

    def start(self, block=True, timeout=None):
        """ Start a kernel controller and eventually start a fresh one if the previous one was terminated.
        
        Set `block` to :data:`True` (default is :data:`False`) to wait for the kernel to be up and running 
        before returning. Optionally, set a timeout in seconds. If the timeout is reached, a :data:`TimeoutError`
        will be raised and the kernel is terminated.
        """
        try:
            future = self.start_future()
            if future and block:
                future.result(timeout=timeout)
        except Exception as e:
            try:
                self.terminate()
            finally:
                raise e

    def start_future(self):
        """ Request the Wolfram kernel to start and return a future object.
        
        The result of the future object is :data:`True` when the kernel is ready to evaluate input."""
        self.stopped = False
        if self.kernel_controller.terminated:
            self.kernel_controller = self.kernel_controller.duplicate()
        if not self.started:
            return self.kernel_controller.request_kernel_start()
        future = futures.Future()
        future.set_result(True)
        return future

    def stop(self):
        """ Request the Wolfram kernel to stop gracefully. """
        self._stop(gracefully=True)

    def terminate(self):
        """ Request the Wolfram kernel to stop immediately.
        
        Ongoing evaluations may be cancelled. """
        self._stop(gracefully=False)

    def _stop(self, gracefully=True):
        # if the kernel is terminated the queue no more accept new tasks. Stop would hang.
        if not self.stopped:
            future = self.stop_future(gracefully=gracefully)
            future.result()

    def stop_future(self, gracefully=True):
        """ Request the Wolfram kernel to stop and return a future object.
        
        The result of the future object is :data:`True` when the controller thread is no longer alive.
        Set `gracefully` to :data:`False` to request an immediate stop, eventually cancelling ongoing
        evaluations.
        """
        self.stopped = True
        if gracefully:
            return self.kernel_controller.stop()
        else:
            return self.kernel_controller.terminate()

    def ensure_started(self):
        if not self.started:
            self.start(block=True, timeout=None)
        if self.stopped:
            self.restart()

    def restart(self, block=True, timeout=None):
        """ Restart a given evaluator by stopping it in cases where it is already started. """
        if self.started:
            self.stop()
        self.start(block=block, timeout=timeout)

    CALLBACK_GET_WXF = staticmethod(do_get_wxf)
    CALLBACK_GET = staticmethod(do_get_result)

    def do_evaluate_future(self, expr, result_update_callback=None, **kwargs):
        future = futures.Future()
        wxf = export(self.normalize_input(expr), target_format='wxf', **kwargs)
        self.kernel_controller.evaluate_future(
            wxf,
            future,
            result_update_callback=result_update_callback,
            **kwargs)
        return future

    def evaluate_future(self, expr, **kwargs):
        """ Evaluate an expression and return a future object.

        The future object result is the evaluated expression. See
        :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate`.
        """
        self.ensure_started()
        return self.do_evaluate_future(
            expr, result_update_callback=self.CALLBACK_GET, **kwargs)

    def evaluate_wxf_future(self, expr, **kwargs):
        """ Evaluate an expression and return a future object.

        The future object result is the WXF serialization of the evaluated expression.
        See :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate_wxf`.
        """
        self.ensure_started()
        return self.do_evaluate_future(
            expr, result_update_callback=self.CALLBACK_GET_WXF, **kwargs)

    def evaluate_wrap_future(self, expr, **kwargs):
        """ Evaluate an expression and return a future object.

        The future object result is the result object with the evaluated expression and meta information.
        See :func:`~wolframclient.evaluation.WolframLanguageSession.evaluate_wrap`.
        """
        self.ensure_started()
        return self.do_evaluate_future(expr, **kwargs)

    def evaluate_wrap(self, expr, **kwargs):
        return self.evaluate_wrap_future(expr, **kwargs).result()

    def evaluate(self, expr, **kwargs):
        result = self.evaluate_wrap(expr, **kwargs)
        self.log_message_from_result(result)
        return result.get()

    def evaluate_wxf(self, expr, **kwargs):
        """ Evaluate an expression and return the serialized expression. 
        
        This method does not deserialize the Wolfram kernel input. """
        result = self.evaluate_wrap(expr, **kwargs)
        self.log_message_from_result(result)
        return result.wxf

    def log_message_from_result(self, result):
        if not result.success:
            for msg in result.messages:
                logger.warning(msg)

    def get_parameter(self, parameter_name):
        return self.kernel_controller.get_parameter(parameter_name)

    def set_parameter(self, parameter_name, parameter_value):
        self.kernel_controller.set_parameter(parameter_name, parameter_value)

    get_parameter.__doc__ = WolframKernelController.get_parameter.__doc__
    set_parameter.__doc__ = WolframKernelController.set_parameter.__doc__

    def __repr__(self):
        if self.started:
            return '<%s: kernel controller=%s>' % (self.__class__.__name__,
                                                   self.kernel_controller)
        else:
            return '<%s: not started>' % self.__class__.__name__
