# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
from asyncio import CancelledError

from wolframclient.evaluation.kernel.asyncsession import (
    WolframLanguageAsyncSession)
from wolframclient.exception import WolframKernelException
from wolframclient.utils.api import asyncio

logger = logging.getLogger(__name__)

__all__ = ['WolframKernelPool', 'parallel_evaluate']


class WolframKernelPool(object):
    """ A pool of kernels to dispatch one-shot evaluations asynchronously.

    `poolsize` is the number of kernel instances. Beware of licencing limits and choose this parameter accordingly.
    `load_factor` indicate how many workloads are queued per kernel before put operation blocks. Values below or equal to 0 means infinite queue size.
    `loop` event loop to use.
    `kwargs` are passed to :class:`wolframclient.evaluation.WolframLanguageAsyncSession` during initialization.
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
        self._loop = loop or asyncio.get_event_loop()
        self._queue = asyncio.Queue(load_factor * poolsize, loop=self._loop)
        self._kernels = {
            WolframLanguageAsyncSession(kernelpath, loop=self._loop, **kwargs)
            for _ in range(poolsize)
        }
        self._started_tasks = []
        self._pending_init_tasks = None
        self.last = 0
        self.eval_count = 0
        self.requestedsize = poolsize

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
            except CancelledError as cancel:
                logger.warning('Loop associated to kernel %s cancelled.',
                               kernel)
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
        kernel_started = False
        try:
            # start the kernel
            await kernel.async_start()
            kernel_started = True
        except asyncio.CancelledError:
            logger.info('Cancelled signal during kernel start.')
        except Exception as e:
            try:
                if logger.isEnabledFor(logging.INFO):
                    logger.info(
                        'A kernel from pool failed to start: %s. Reason is %s',
                        kernel, e)
                await kernel.async_terminate()
            except asyncio.CancelledError:
                logger.info('Cancelled signal.')
            except Exception as e2:
                logger.info(
                    'Exception raised during clean-up after failed start: %s',
                    e2)

        if kernel_started:
            # shedule the infinite evaluation loop
            task = asyncio.ensure_task(
                self._kernel_loop(kernel), loop=self._loop)
            if logger.isEnabledFor(logging.INFO):
                logger.info('New kernel started in pool: %s.', kernel)
            # register the task. The loop is not always started at this point.
            self._started_tasks.append(task)

    async def start(self):
        """ Start a pool of kernels and wait for at least one of them to 
        be ready for evaluation.

        This method is a coroutine.
        If not all the kernels were able to start fails and terminate the pool.
        """
        # keep track of the init tasks. We have to wait before terminating.
        self._pending_init_tasks = {(asyncio.ensure_task(
            self._async_start_kernel(kernel)))
                                    for kernel in self._kernels}
        # uninitialized kernels are removed if they failed to start
        # if they do start the task (the loop) is added to _started_tasks.
        # we need at least one working kernel.
        # we also need to keep track of start kernel tasks in case of early termination.
        while len(self._started_tasks) == 0:
            _, self._pending_init_tasks = await asyncio.wait(
                self._pending_init_tasks, return_when=asyncio.FIRST_COMPLETED)
            if len(self._pending_init_tasks) == 0:
                raise WolframKernelException('Failed to start any kernel.')
        logger.info('Pool initialized with %i running kernels',
                    len(self._started_tasks))

    async def terminate(self):
        logger.info('Start terminate pool')
        for kernel in self._kernels:
            kernel._abort()
        # make sure all init tasks are finished.
        if len(self._pending_init_tasks) > 0:
            for task in self._pending_init_tasks:
                task.cancel()
            await asyncio.wait(self._pending_init_tasks)
        if len(self._started_tasks) > 0:
            try:
                # request for loop termination.
                for _ in range(len(self._started_tasks)):
                    await self._queue.put(None)
                # wait for loop to finish before terminating the kernels
                await asyncio.wait(self._started_tasks, loop=self._loop)
            except CancelledError:
                pass
            except Exception as e:
                logger.warning('Exception raised while terminating loop: %s',
                               e)
        # terminate the kernel instances, if any started.
        tasks = {
            asyncio.ensure_task(kernel.async_terminate())
            for kernel in self._kernels
        }
        logger.info('Call terminate from kernelpool')
        # `wait` raises the first exception, but wait for all tasks to finish.
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

    def evaluate_all(self, iterable):
        return self._loop.run_until_complete(self._evaluate_all(iterable))

    async def _evaluate_all(self, iterable):
        tasks = [asyncio.ensure_task(self.evaluate(expr)) for expr in iterable]
        return await asyncio.gather(*tasks)

    def __repr__(self):
        return 'WolframKernelPool<started %i/%i kernels cummulating %i evaluations>' % (
            len(self._started_tasks), self.requestedsize, self.eval_count)


def parallel_evaluate(kernelpath, expressions, max_kernels=6, loop=None):
    """ Start a kernel pool using `kernelpath` and evaluate the expressions on the created
    pool, then terminate it and returns the results.

    Note that each evaluation should be independent and not rely on any previous one, since
    there is no guarantee that two given expr evaluates on the same kernel, and scoped to 
    avoid side effects.
    """
    if loop is None:
        loop = asyncio.get_event_loop()
    pool = None
    try:
        pool = WolframKernelPool(kernelpath, poolsize=max_kernels, loop=loop)
        loop.run_until_complete(pool.start())
        return pool.evaluate_all(expressions)
    finally:
        if pool:
            loop.run_until_complete(pool.terminate())
