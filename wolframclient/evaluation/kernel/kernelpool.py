# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging

from wolframclient.evaluation.kernel.asyncsession import (
    WolframLanguageAsyncSession)
from wolframclient.utils.api import asyncio

logger = logging.getLogger(__name__)

__all__ = ['WolframKernelPool']


class WolframKernelPool(object):
    """ A pool of kernels to dispatch one-shot evaluations asynchronously.

    `poolsize` is the number of kernel instances. Beware of licencing limits and choose this parameter accordingly.
    load_factor indicate how many workloads are queued before put operation blocks. Values below or equal to 0 means infinite queue size.
    loop event loop to use.
    """

    def __init__(self,
                 kernelpath,
                 poolsize=4,
                 load_factor=0,
                 loop=None,
                 **kwargs):
        if poolsize <= 0:
            raise ValueError(
                'Invalid pool size value %i. Expecting a positive integer.' %
                i)
        self._loop = loop or asyncio.get_running_loop()
        self._queue = asyncio.Queue(load_factor * poolsize, loop=self._loop)
        self._kernels = {
            WolframLanguageAsyncSession(kernelpath, loop=self._loop)
            for _ in range(poolsize)
        }
        self._init_tasks = []
        self._started_tasks = []
        self.last = 0
        self.eval_count = 0
        self.requestedsize = poolsize
        for k, v in kwargs.items():
            for kernel in self._kernels:
                kernel.set_parameter(k, v)

    async def _kernel_loop(self, kernel):
        while True:
            try:
                future = None
                task = None
                logger.debug('Wait for a new queue entry.')
                task = await self._queue.get()
                if task is None:
                    logger.info(
                        'Termination requested for kernel: %s.' % kernel)
                    break
                # func is one of the evaluate* methods from WolframLanguageAsyncSession.
                future, func, args, kwargs = task
                logger.debug('Processing expr %s' % args)
                # those method can't be canceled since the kernel is evaluating anyway.
                try:
                    result = await asyncio.shield(
                        func(kernel, *args, **kwargs))
                    future.set_result(result)
                except Exception as e:
                    future.set_exception(e)
            # First exceptions are those we can't recover from.
            except KeyboardInterrupt as interrupt:
                logger.error(
                    'Loop associated to kernel %s interrupted by user.',
                    kernel)
                raise interrupt
            except asyncio.CancelledError as cancel:
                logger.warn('Loop associated to kernel %s cancelled.', kernel)
                raise cancel
            except RuntimeError as runtime:
                logger.error('Unexpected runtime error: {}', runtime)
                raise runtime
            except Exception as e:
                if future:
                    logger.warning(
                        'Exception raised in loop returned in future object. Exception was: %s'
                        % e)
                    future.set_exception(e)
                else:
                    logger.warning(
                        'No future object. Exception raised in loop was: %s' %
                        e)
                    raise e
            finally:
                if task:
                    self._queue.task_done()

    def __enter__(self):
        """ A user friendly message when 'async with' is not used. """
        raise NotImplementedError("%s must be used in a 'async with' block." %
                                  self.__class__.__name__)

    def __exit__(self, type, value, traceback):
        """ Let the __enter__ method fail and propagate doing nothing. """

    async def __aenter__(self):
        """Awaitable start"""
        await self.start()
        return self

    async def __aexit__(self, type, value, traceback):
        """Awaitable terminate the kernel process and close sockets."""
        await self.terminate()

    async def _async_start_kernel(self, kernel):
        try:
            # start the kernel
            await kernel.async_start()
            # shedule the infinite evaluation loop
            task = asyncio.ensure_task(
                self._kernel_loop(kernel), loop=self._loop)
            # register the task. The loop is not always started at this point.
            self._started_tasks.append(task)
        except Exception as e:
            try:
                logger.fatal('A kernel failed to start. %s', e)
                await kernel.async_terminate()
            except Exception as e2:
                logger.warn(
                    'Exception raised during clean-up after failed start: %s',
                    e2)
            finally:
                self._kernels.remove(kernel)

    async def start(self):
        """ Start a pool of kernels and wait for them to be ready for evaluation.

        This method is a coroutine.
        If not all the kernels were able to start fails and terminate the pool.
        """
        self._init_tasks = {
            asyncio.ensure_task(self._async_start_kernel(kernel))
            for kernel in self._kernels
        }

    async def terminate(self):
        if len(self._started_tasks) > 0:
            try:
                # request for loop termination.
                for _ in range(len(self._started_tasks)):
                    await self._queue.put(None)
                # wait for loop to finish before terminating the kernels
                await asyncio.wait(self._started_tasks, loop=self._loop)
            except asyncio.CancelledError:
                pass
            except Exception as e:
                logger.warn('Exception raised while terminating loop: %s', e)
        # terminate the kernel instances.
        tasks = {
            asyncio.ensure_task(kernel.async_terminate())
            for kernel in self._kernels
        }
        # Raise the first exception, but wait for all tasks to finish.
        await asyncio.wait(tasks, loop=self._loop)

    async def _put_evaluation_task(self, future, func, expr, **kwargs):
        await self._queue.put((future, func, (expr, ), kwargs))
        self.eval_count += 1

    async def evaluate(self, expr, **kwargs):
        future = asyncio.Future(loop=self._loop)
        await self._put_evaluation_task(
            future, WolframLanguageAsyncSession.evaluate, expr, **kwargs)
        return await future

    async def evaluate_wxf(self, expr, **kwargs):
        future = asyncio.Future(loop=self._loop)
        await self._put_evaluation_task(
            future, WolframLanguageAsyncSession.evaluate_wxf, expr, **kwargs)
        return await future

    async def evaluate_wrap(self, expr, **kwargs):
        future = asyncio.Future(loop=self._loop)
        await self._put_evaluation_task(
            future, WolframLanguageAsyncSession.evaluate_wrap, expr, **kwargs)
        return await future

    def __repr__(self):
        return 'WolframKernelPool<started %i/%i kernels cummulating %i evaluations>' % (
            len(self._started_tasks), self.requestedsize, self.eval_count)
