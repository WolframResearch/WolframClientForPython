# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import asyncio
import logging

from wolframclient.evaluation.kernel.kernelsession import WolframLanguageAsyncSession

TerminateTask = object()

logger = logging.getLogger(__name__)

class WolframKernelPool(object):
    """ A pool of kernels to dispatch one-shot evaluations asynchronously.

    poolsize number of kernel instance. Beware of licencing limits.
    load_factor indicate how many workloads are queued before put operation blocks. Values below or equal to 0 means infinite queue size.
    loop event loop to use.
    """

    def __init__(self, kernelpath, poolsize=4, load_factor=0, loop=None, **kwargs):
        if poolsize <= 0:
            raise ValueError(
                'Invalid pool size value %i. Expecting a positive integer.' % i)
        self._loop = loop or asyncio.get_event_loop()
        self._queue = asyncio.Queue(load_factor * poolsize, loop=self._loop)
        self._kernels = [WolframLanguageAsyncSession(
            kernelpath) for _ in range(poolsize)]
        self._tasks = None
        self.last = 0
        self.eval_count = 0
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
                if task is TerminateTask:
                    logger.info(
                        'Termination requested for kernel: %s.' % kernel)
                    break
                # func is one of the async_evaluate* methods from WolframLanguageAsyncSession.
                future, func, args, kwargs = task
                logger.debug('Processing expr %s' % args)
                # those method can't be canceled since the kernel is evaluating anyway.
                try:
                    result = await asyncio.shield(func(kernel, *args, loop=self._loop, **kwargs))
                    future.set_result(result)
                except Exception as e:
                    future.set_exception(e)
            # First exceptions are those we can't recover from.
            except KeyboardInterrupt as interrupt:
                logger.error(
                    'Loop associated to kernel {} interrupted by user.', kernel)
                raise interrupt
            except asyncio.CancelledError as cancel:
                logger.error('Loop associated to kernel {} cancelled.', kernel)
                raise cancel
            except RuntimeError as runtime:
                logger.error('Unexpected runtime error: {}', runtime)
                raise runtime
            except Exception as e:
                if future:
                    logger.warning(
                        'Exception raised in loop returned in future object. Exception was: %s' % e)
                    future.set_exception(e)
                else:
                    logger.warning(
                        'No future object. Exception raised in loop was: %s' % e)
                    raise e
            finally:
                if task:
                    self._queue.task_done()

    async def __aenter__(self):
        """Awaitable start"""
        await self.start()
        return self

    async def __aexit__(self, type, value, traceback):
        """Awaitable terminate the kernel process and close sockets."""
        await self.terminate()

    async def start_kernel(self, kernel):
        await kernel.async_start(loop=self._loop)
        # start evaluation task processing in the background.
        return asyncio.ensure_future(self._kernel_loop(kernel), loop=self._loop)

    async def start(self):
        try:
            tasks = {asyncio.ensure_future(self.start_kernel(kernel))
                     for kernel in self._kernels}
            self._task = await asyncio.gather(*tasks, loop=self._loop)
        except Exception as e:
            try:
                logger.fatal(
                    'Kernel pool failed to start the requested amount of kernels.')
                await self._terminate_kernels()
            except Exception as e2:
                logger.warn(
                    'Exception raised during clean-up after failed start: %s' % e2)
            raise e

    async def terminate(self):
        try:
            for _ in range(len(self._kernels)):
                await self._queue.put(TerminateTask)
        except asyncio.CancelledError:
            pass
        except Exception as e:
            logger.warn('Exception raised while terminating loop: %s', e)

        # wait for loop to finish before killing the kernels
        if self._tasks:
            await asyncio.wait(self._tasks, loop=self._loop)
        await self._terminate_kernels()

    async def _terminate_kernels(self):
        tasks = {asyncio.ensure_future(
            kernel.async_terminate(loop=self._loop)
        ) for kernel in self._kernels
        }
        await asyncio.wait(tasks, loop=self._loop)

    async def _put_evaluation_task(self, future, func, expr, **kwargs):
        await self._queue.put((future, func, (expr,), kwargs))
        self.eval_count += 1

    async def evaluate(self, expr, **kwargs):
        future = asyncio.Future(loop=self._loop)
        await self._put_evaluation_task(future, WolframLanguageAsyncSession.async_evaluate, expr, **kwargs)
        return await future

    async def evaluate_wxf(self, expr, **kwargs):
        future = asyncio.Future(loop=self._loop)
        await self._put_evaluation_task(future, WolframLanguageAsyncSession.async_evaluate_wxf, expr, **kwargs)
        return await future

    async def evaluate_wrap(self, expr, **kwargs):
        future = asyncio.Future(loop=self._loop)
        await self._put_evaluation_task(future, WolframLanguageAsyncSession.async_evaluate_wrap, expr, **kwargs)
        return await future

    def __repr__(self):
        return 'WolframKernelPool<%i kernels cummulating %i evaluations>' % (len(self._kernels), self.eval_count)
