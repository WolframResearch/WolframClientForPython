# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
from subprocess import PIPE
from threading import Event

from wolframclient.evaluation.kernel.kernelsession import (
    WolframLanguageSession)
from wolframclient.utils.api import asyncio, futures

logger = logging.getLogger(__name__)

__all__ = ['WolframLanguageAsyncSession']


class WolframLanguageAsyncSession(WolframLanguageSession):
    """Evaluate expressions asynchronously using coroutines.

    Asynchronous evaluations are provided through coroutines and the :mod:`asyncio` modules.

    Instance of this class can be started using both with a synchronous context manager::

        async def coroutine1(kernelpath): 
            with WolframLanguageAsyncSession(kernelpath) as session:
                await session.evaluate('Now')

    or with an asynchronous context manager::
        
        async def coroutine2(kernelpath): 
            async with WolframLanguageAsyncSession(kernelpath) as session:
                await session.evaluate('Now')


    Evaluation methods herited from :class:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession` 
    are coroutines, namely: 
    :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageAsyncSession.evaluate`,
    :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageAsyncSession.evaluate_wxf`, and
    :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageAsyncSession.evaluate_wrap`. 

    An event loop can be provided as `loop`, otherwise :meth:`asynchio.get_event_loop` is called.
    """

    def __init__(self,
                 kernel,
                 consumer=None,
                 loop=None,
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
            kernel,
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
        self._loop = loop or asyncio.get_event_loop()
        # event shared with timed out socket read function, to cancel
        # zmq operation at startup.
        self.event_abort = Event()

    def _socket_read_sleep_func(self, duration):
        if not self.event_abort.is_set():
            self.event_abort.wait(timeout=duration)
        else:
            raise TimeoutError

    def _abort(self):
        if not self.started:
            self.event_abort.set()

    def _get_exec_pool(self):
        if self.thread_pool_exec is None:
            self.thread_pool_exec = futures.ThreadPoolExecutor(max_workers=1)
        return self.thread_pool_exec

    async def evaluate(self, expr, **kwargs):
        """Evaluate :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate`
        asynchronously.

        This method is a coroutine."""
        return await self._async_evaluate(super().evaluate, expr, **kwargs)

    async def evaluate_wxf(self, expr, **kwargs):
        """Evaluate :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate_wxf`
        asynchronously.

        This method is a coroutine."""
        return await self._async_evaluate(super().evaluate_wxf, expr, **kwargs)

    async def evaluate_wrap(self, expr, **kwargs):
        """Evaluate :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate_wrap`
        asynchronously.

        This method is a coroutine."""
        return await self._async_evaluate(super().evaluate_wrap, expr,
                                          **kwargs)

    async def _async_evaluate(self, func, expr, **kwargs):
        return await self._loop.run_in_executor(self._get_exec_pool(), func,
                                                expr, **kwargs)

    async def start(self):
        """Asynchronously start the session.
        
        This method is a coroutine."""
        try:
            await self._loop.run_in_executor(self._get_exec_pool(), super()._start)
        except Exception as e:
            await self.terminate()
            raise e


    async def stop(self):
        await self._async_terminate(super().stop, True)
    
    async def terminate(self):
        await self._async_terminate(super().terminate, False)

    async def _async_terminate(self, sync_stop_func, wait):
        """Asynchronously terminate the session.
        
        This method is a coroutine."""
        # Check that the thread pool was created, otherwise it's an initialized session
        # that was never started, and use it to asynchronously stop the kernel.
        if self.thread_pool_exec:
            try:
                logger.info('Terminating asynchronous kernel session.')
                await self._loop.run_in_executor(self._get_exec_pool(), sync_stop_func)
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
