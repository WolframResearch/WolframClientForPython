# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
import asyncio
import logging
from subprocess import PIPE
from wolframclient.utils.api import futures
from wolframclient.evaluation.kernel.kernelsession import WolframLanguageSession

logger = logging.getLogger(__name__)

__all__ = ['WolframLanguageAsyncSession']

class WolframLanguageAsyncSession(WolframLanguageSession):
    """Evaluate expressions asynchronously.

    Asynchronous evaluations are provided using two abstractions, either through coroutines,
    or through the :mod:`~concurrent` module and the :class:`~concurrent.futures.Future` class. 

    Instance of this class can be started using both synchronous and asynchronous context management.

    Evaluation methods prefixed with `async_` are coroutines, namely: 
    :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageAsyncSession.async_evaluate`,
    :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageAsyncSession.async_evaluate_wxf`, and
    :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageAsyncSession.async_evaluate_wrap`. 
    
    Evaluation methods herited from :class:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession`,
    namely :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageAsyncSession.evaluate`,
    :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageAsyncSession.evaluate_wxf`, and 
    :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageAsyncSession.evaluate_wrap`,
    run asynchronously and return :class:`~concurrent.futures.Future` objects.

    A `loop` can be provided, otherwise asynchio.get_event_loop will be used, if available.
    """

    def __init__(self, kernel, consumer=None, loop=None, initfile=None,
                 in_socket=None, out_socket=None, kernel_loglevel=logging.NOTSET, stdin=PIPE, stdout=PIPE, stderr=PIPE):
        super(WolframLanguageAsyncSession, self).__init__(kernel, consumer=consumer, initfile=initfile,
                                                          in_socket=in_socket, out_socket=out_socket,
                                                          kernel_loglevel=kernel_loglevel,
                                                          stdin=stdin, stdout=stdout, stderr=stderr)
        self.thread_pool_exec = None
        self._loop = loop

    def evaluate(self, expr, **kwargs):
        """Evaluate :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate`
        asynchronously and return a :class:`~concurrent.futures.Future` object."""
        return self._do_in_thread(super().evaluate, expr, **kwargs)

    def evaluate_wxf(self, expr, **kwargs):
        """Evaluate :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate_wxf`
        asynchronously and return a :class:`~concurrent.futures.Future` object."""
        return self._do_in_thread(super().evaluate_wxf, expr, **kwargs)

    def evaluate_wrap(self, expr, **kwargs):
        """Evaluate :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate_wrap`
        asynchronously and return a :class:`~concurrent.futures.Future` object."""
        return self._do_in_thread(super().evaluate_wrap, expr, **kwargs)

    def _do_in_thread(self, func, *args, **kwargs):
        try:
            self._get_exec_pool()
            return self.thread_pool_exec.submit(func, *args, **kwargs)
        except ImportError:
            logger.fatal('Module concurrent.futures is missing.')
            raise NotImplementedError(
                'Asynchronous evaluation is not available on this Python interpreter.')

    def _get_exec_pool(self):
        if self.thread_pool_exec is None:
            self.thread_pool_exec = futures.ThreadPoolExecutor(
                    max_workers=1)
        return self.thread_pool_exec

    def _get_loop(self):
        # we only need a loop when coroutines are involved.
        if self._loop is None:
            self._loop = asyncio.get_event_loop()
        return self._loop

    async def async_evaluate(self, expr, **kwargs):
        """Evaluate :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate`
        asynchronously.

        This method is a coroutine."""
        return await self._async_evaluate(super().evaluate, expr, **kwargs)

    async def async_evaluate_wxf(self, expr, **kwargs):
        """Evaluate :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate_wxf`
        asynchronously.

        This method is a coroutine."""
        return await self._async_evaluate(super().evaluate_wxf, expr, **kwargs)

    async def async_evaluate_wrap(self, expr, **kwargs):
        """Evaluate :meth:`~wolframclient.evaluation.kernel.kernelsession.WolframLanguageSession.evaluate_wrap`
        asynchronously.

        This method is a coroutine."""
        return await self._async_evaluate(super().evaluate_wrap, expr, **kwargs)

    async def _async_evaluate(self, func, expr, **kwargs):
        return await self._get_loop().run_in_executor(self._get_exec_pool(), func, expr, **kwargs)

    async def __aenter__(self):
        await self.async_start()
        return self

    async def __aexit__(self, type, value, traceback):
        await self.async_terminate()

    async def async_start(self):
        """Asynchronously start the session.
        
        This method is a coroutine."""
        await self._get_loop().run_in_executor(self._get_exec_pool(), super().start)

    async def async_terminate(self):
        """Asynchronously terminate the session.
        
        This method is a coroutine."""
        # make sure the session was started. Otherwise it's an initialized session
        # that was never started.
        if self.thread_pool_exec:
            try:
                await self._get_loop().run_in_executor(self._get_exec_pool(), super().terminate)
            except Exception as e:
                logger.info(
                    'Failed to terminate kernel asynchronously: %s' % e)
            finally:
                # Loop may not be available, fallback to synchronous termination.
                try:
                    self.thread_pool_exec.shutdown(wait=True)
                except Exception as e:
                    logger.info(
                        'Error when shutting down thread pool executor: %s' % e)
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
