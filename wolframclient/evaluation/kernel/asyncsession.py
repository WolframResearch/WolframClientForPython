# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
from subprocess import PIPE

from wolframclient.evaluation.base import WolframAsyncEvaluator
from wolframclient.evaluation.kernel.localsession import WolframLanguageSession
from wolframclient.utils.api import asyncio

logger = logging.getLogger(__name__)

__all__ = ['WolframLanguageAsyncSession']


class WolframLanguageAsyncSession(WolframAsyncEvaluator,
                                  WolframLanguageSession):
    """Evaluate expressions asynchronously using coroutines.

    Asynchronous evaluations are provided through coroutines and the :mod:`asyncio` modules.

    Instances of this class can be managed with an asynchronous context manager::
        
        async with WolframLanguageAsyncSession() as session:
            await session.evaluate('Now')

    An event loop can be explicitly passed using the named parameter `loop`; otherwise, the one
    returned by :func:`~asyncio.get_event_loop` is used.

    Coroutines all run in a unique thread. Since a Wolfram kernel is single threaded, there can
    be only one evaluation at a time. In a sense, from the event loop point of view, evaluations 
    are atomic operations. Even when many asynchronous sessions are started, the number of 
    threads equals the number of kernel instances running and should not be problematic. Ensuring 
    that only one thread runs all operations of a given Wolfram kernel significantly reduces the
    complexity of the code without any real drawback.
    """

    def __init__(self,
                 kernel=None,
                 consumer=None,
                 loop=None,
                 initfile=None,
                 kernel_loglevel=logging.NOTSET,
                 stdin=PIPE,
                 stdout=PIPE,
                 stderr=PIPE,
                 inputform_string_evaluation=True,
                 **kwargs):
        super().__init__(
            kernel=kernel,
            consumer=consumer,
            initfile=initfile,
            kernel_loglevel=kernel_loglevel,
            stdin=stdin,
            stdout=stdout,
            stderr=stderr,
            inputform_string_evaluation=inputform_string_evaluation,
            loop=loop,
            **kwargs)

    def duplicate(self):
        return self.__class__(
            kernel=self.kernel,
            loop=self._loop,
            consumer=self.consumer,
            initfile=self.initfile,
            kernel_loglevel=self.kernel_loglevel,
            stdin=self._stdin,
            stdout=self._stdout,
            stderr=self._stderr,
            inputform_string_evaluation=self.inputform_string_evaluation,
            **self.parameters)

    async def do_evaluate_future(self,
                                 expr,
                                 result_update_callback=None,
                                 **kwargs):
        future = super().do_evaluate_future(
            expr, result_update_callback=result_update_callback, **kwargs)
        return asyncio.wrap_future(future, loop=self._loop)

    async def evaluate_future(self, expr, **kwargs):
        await self.ensure_started()
        return await self.do_evaluate_future(
            expr, result_update_callback=self.CALLBACK_GET, **kwargs)

    async def evaluate_wxf_future(self, expr, **kwargs):
        await self.ensure_started()
        return await self.do_evaluate_future(
            expr, result_update_callback=self.CALLBACK_GET_WXF, **kwargs)

    async def evaluate_wrap_future(self, expr, **kwargs):
        await self.ensure_started()
        return await self.do_evaluate_future(expr, **kwargs)

    async def evaluate(self, expr, **kwargs):
        """Evaluate an expression asynchronously.

        This method is a coroutine."""
        future = await self.evaluate_future(expr, **kwargs)
        await future
        return future.result()

    async def evaluate_wxf(self, expr, **kwargs):
        """Evaluate an expression and return the WXF output asynchronously.

        This method is a coroutine."""
        future = await self.evaluate_wxf_future(expr, **kwargs)
        await future
        return future.result()

    async def evaluate_wrap(self, expr, **kwargs):
        """Evaluate an expression and return a result object asynchronously.

        This method is a coroutine."""
        future = await self.evaluate_wrap_future(expr, **kwargs)
        await future
        return future.result()

    async def evaluate_many(self, expr_list):
        return await super().evaluate_many(expr_list)

    async def ensure_started(self):
        if not self.started:
            await self.start()
        if self.stopped:
            await self.restart()

    async def restart(self):
        if self.started:
            await self.stop()
        await self.start()

    async def start(self):
        """Asynchronously start the session.
        
        This method is a coroutine."""
        future = super().start_future()
        await asyncio.wrap_future(future, loop=self._loop)

    async def stop(self):
        """Asynchronously stop the session (graceful termination).
        
        This method is a coroutine."""
        await self._async_terminate(True)

    async def terminate(self):
        """Asynchronously terminate the session.
        
        This method is a coroutine."""
        await self._async_terminate(False)

    async def _async_terminate(self, gracefully):
        logger.info('Terminating asynchronous kernel session.')
        future = super().stop_future(gracefully=gracefully)
        await asyncio.wrap_future(future, loop=self._loop)
