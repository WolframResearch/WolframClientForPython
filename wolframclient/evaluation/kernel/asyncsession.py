# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
from subprocess import PIPE

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
                 **kwargs):
        super(WolframLanguageAsyncSession, self).__init__(
            kernel,
            consumer=consumer,
            initfile=initfile,
            in_socket=in_socket,
            out_socket=out_socket,
            kernel_loglevel=kernel_loglevel,
            stdin=stdin,
            stdout=stdout,
            stderr=stderr,
            **kwargs)
        self.thread_pool_exec = None
        self._loop = loop or asyncio.get_event_loop()

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

    async def __aenter__(self):
        await self.async_start()
        return self

    async def __aexit__(self, type, value, traceback):
        await self.async_terminate()

    async def async_start(self):
        """Asynchronously start the session.
        
        This method is a coroutine."""
        await self._loop.run_in_executor(self._get_exec_pool(), super().start)

    async def async_terminate(self):
        """Asynchronously terminate the session.
        
        This method is a coroutine."""
        # make sure the session was started. Otherwise it's an initialized session
        # that was never started.
        if self.thread_pool_exec:
            try:
                await self._loop.run_in_executor(self._get_exec_pool(),
                                                 super().terminate)
            except Exception as e:
                logger.info(
                    'Failed to terminate kernel asynchronously: %s' % e)
            finally:
                # Loop may not be available, fallback to synchronous termination.
                try:
                    self.thread_pool_exec.shutdown(wait=True)
                except Exception as e:
                    logger.info(
                        'Error when shutting down thread pool executor: %s' %
                        e)
        elif self.started:
            self.termiate()

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
