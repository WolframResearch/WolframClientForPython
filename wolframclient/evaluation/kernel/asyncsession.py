# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
from subprocess import PIPE
from threading import Event
from functools import partial
from wolframclient.evaluation.base import WolframAsyncEvaluator
from wolframclient.evaluation.kernel.localsession import WolframLanguageSession
from wolframclient.utils.api import asyncio, futures, time

logger = logging.getLogger(__name__)

__all__ = ['WolframLanguageAsyncSession']


class WolframLanguageAsyncSession(WolframLanguageSession,
                                  WolframAsyncEvaluator):
    """Evaluate expressions asynchronously using coroutines.

    Asynchronous evaluations are provided through coroutines and the :mod:`asyncio` modules.

    Instances of this class can be managed with an asynchronous context manager::
        
        async with WolframLanguageAsyncSession() as session:
            await session.evaluate('Now')

    An event loop can be explicitly passed using the named parameter `loop`, otherwise the one 
    returned by :func:`~asyncio.get_event_loop` is used.

    Coroutines all run in a unique thread. Since a Wolfram Kernel is single threaded, there can
    be only one evaluation at a time. In a sense, from the event loop point of view, evaluations 
    are atomic operations. Even when many asynchronous sessions are started, the number of 
    threads equals the number of kernel instances running and should not be problematic. Ensuring 
    that only one thread runs all operations of a given Wolfram kernel, significantly reduces the 
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
                 wxf_bytes_evaluation=True,
                 **kwargs):
        self._loop = loop or asyncio.get_event_loop()
        super().__init__(
            kernel=kernel,
            consumer=consumer,
            initfile=initfile,
            kernel_loglevel=kernel_loglevel,
            stdin=stdin,
            stdout=stdout,
            stderr=stderr,
            inputform_string_evaluation=inputform_string_evaluation,
            wxf_bytes_evaluation=wxf_bytes_evaluation,
            **kwargs)
        self.thread_pool_exec = None

    def duplicate(self):
        return self.__class__(
            self.kernel,
            loop=self._loop,
            consumer=self.consumer,
            initfile=self.initfile,
            kernel_loglevel=self.loglevel,
            stdin=self._stdin,
            stdout=self._stdout,
            stderr=self._stderr,
            inputform_string_evaluation=self.inputform_string_evaluation,
            wxf_bytes_evaluation=self.wxf_bytes_evaluation,
            **self.parameters)

    def _get_exec_pool(self):
        if self.thread_pool_exec is None:
            self.thread_pool_exec = futures.ThreadPoolExecutor(max_workers=1)
        return self.thread_pool_exec

    async def do_evaluate_future(self, expr, result_update_callback=None, **kwargs):
        future = super().do_evaluate_future(expr, result_update_callback=result_update_callback, **kwargs)
        return asyncio.wrap_future(future, loop=self._loop)

    async def evaluate_future(self, expr, **kwargs):
        await self.ensure_started()
        return await self.do_evaluate_future(expr, result_update_callback=self.CALLBACK_GET, **kwargs)
    
    async def evaluate_wxf_future(self, expr, **kwargs):
        await self.ensure_started()
        return await self.do_evaluate_future(expr, result_update_callback=self.CALLBACK_GET_WXF, **kwargs)

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
        return await self.evaluate_wxf_future(expr, **kwargs).result()

    async def evaluate_wrap(self, expr, **kwargs):
        """Evaluate an expression and return a result object asynchronously.

        This method is a coroutine."""
        return await self.evaluate_wrap_future(expr, **kwargs).result()

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
        try:
            await self._loop.run_in_executor(
                self._get_exec_pool(),
                partial(super()._start, block=True),
                )
        except Exception as e:
            await self.terminate()
            raise e

    async def stop(self):
        """Asynchronously stop the session (gracefully termination).
        
        This method is a coroutine."""
        await self._async_terminate(super().stop, True)

    async def terminate(self):
        """Asynchronously terminate the session.
        
        This method is a coroutine."""
        await self._async_terminate(super().terminate, False)

    async def _async_terminate(self, sync_stop_func, wait):
        # Check that the thread pool was created, otherwise it's an initialized session
        # that was never started, and use it to asynchronously stop the kernel.
        if self.thread_pool_exec:
            try:
                logger.info('Terminating asynchronous kernel session.')
                await self._loop.run_in_executor(self._get_exec_pool(),
                                                 sync_stop_func)
            except asyncio.CancelledError:
                logger.info('Cancelled terminate task.')
            except Exception as e:
                logger.info(
                    'Failed to terminate kernel asynchronously: %s' % e)
            # kernel stopped. Joining thread.
            try:
                self.thread_pool_exec.shutdown(wait=wait)
                self.thread_pool_exec = None
            except Exception as e:
                logger.info('Thread pool executor error on shutdown: %s', e)
        # synchronous termination required if the kernel is still not terminated.
        if self.started:
            sync_stop_func()
