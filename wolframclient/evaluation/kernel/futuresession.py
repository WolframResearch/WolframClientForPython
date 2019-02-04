# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
from subprocess import PIPE

from wolframclient.evaluation.kernel.kernelsession import (
    WolframLanguageSession)
from wolframclient.utils.api import futures

logger = logging.getLogger(__name__)

__all__ = ['WolframLanguageFutureSession']


class WolframLanguageFutureSession(WolframLanguageSession):
    """Evaluate expressions asynchronously and return Future instances.

    Asynchronous evaluations are backed by the :mod:`concurrent.futures` module, especially the :class:`~concurrent.futures.Future` class. 

    Contrary to :class:`~wolframclient.evaluation.WolframLanguageAsyncSession`, none of the methods of this class
    is a coroutine. Evaluation methods are synchronous; they start the evaluation as a background task in a thread, and immediately return
    a :class:`~concurrent.futures.Future` objects, that can later be awaited.
    """

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
            kernel=kernel,
            consumer=consumer,
            initfile=initfile,
            in_socket=in_socket,
            out_socket=out_socket,
            kernel_loglevel=kernel_loglevel,
            stdin=stdin,
            stdout=stdout,
            stderr=stderr,
            inputform_string_evaluation=inputform_string_evaluation,
            wxf_bytes_evaluation=wxf_bytes_evaluation,
            **kwargs)
        self.thread_pool_exec = None

    def evaluate(self, expr, **kwargs):
        """Evaluate :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate`
        asynchronously and return a :class:`~concurrent.futures.Future` object."""
        return self._do_in_thread(super().evaluate, self.normalize_input(expr), **kwargs)

    def evaluate_wxf(self, expr, **kwargs):
        """Evaluate :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate_wxf`
        asynchronously and return a :class:`~concurrent.futures.Future` object."""
        return self._do_in_thread(super().evaluate_wxf, self.normalize_input(expr), **kwargs)

    def evaluate_wrap(self, expr, **kwargs):
        """Evaluate :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate_wrap`
        asynchronously and return a :class:`~concurrent.futures.Future` object."""
        return self._do_in_thread(super().evaluate_wrap, self.normalize_input(expr), **kwargs)

    def _do_in_thread(self, func, *args, **kwargs):
        try:
            self._get_exec_pool()
            return self.thread_pool_exec.submit(func, *args, **kwargs)
        except ImportError:
            logger.fatal('Module concurrent.futures is missing.')
            raise NotImplementedError(
                'Asynchronous evaluation is not available on this Python interpreter.'
            )

    def _get_exec_pool(self):
        if self.thread_pool_exec is None:
            self.thread_pool_exec = futures.ThreadPoolExecutor(max_workers=1)
        return self.thread_pool_exec

    def terminate(self):
        """Terminate the current session. This is a synchronous method."""
        # First terminate all executions.
        # Then use the socket to actually quit. Avoid crashes when freeing zmq resources still in use.
        if self.thread_pool_exec:
            try:
                self.thread_pool_exec.shutdown(wait=True)
            except Exception as e:
                logger.fatal('Failed to stop thread pool executor: %s' % e)
        super().terminate()
