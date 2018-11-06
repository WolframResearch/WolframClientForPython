# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
import warnings
from concurrent.futures import ThreadPoolExecutor
import asyncio
from wolframclient.utils.asyncio import wait_all, syncronous_wait_all
from wolframclient.language.expression import WLFunction
from wolframclient.utils.functional import first
from wolframclient.utils.six import PY_36
"""
NOTES

 - wolfram.evaluation.__init__ contains a bare try except.

 - CloudFunction is implemented only for cloud kernels
 - hardcoded class calls should be class attributes
 - all classes should have evaluate, evaluate_many (we should not hardcode ToExpression behaviour in evaluate)
 - you are always checking the input type, while this can help some users it's really preventing users from doing duck typing



 - WolframKernelPool should be constructed using a generator of other kernels class
 - WolframKernal should have a copy method
 - should terminated kernels restart automatically instead of raise an Exception?

"""
class WolframEvaluatorBase:
    @property
    def stopped(self):
        """A boolean indicated that the evaluator stop function has started.
        
        Always synchronous to avoid multiple concurrent stops."""
        raise NotImplementedError

    def __del__(self, _warnings=warnings):
        if not self.stopped:
            if PY_36:
                kwargs = {'source': self}
            else:
                kwargs = {}
            _warnings.warn("Unclosed instance of %s: %s" % (self.__class__.__name__, self),
                ResourceWarning,
                **kwargs)

#SYNC VERSION

class WolframEvaluator(WolframEvaluatorBase):

    def evaluate(self, expr):
        return self.evaluate_wrap(expr).get()

    def evaluate_many(self, expr_list):
        return map(self.evaluate, expr_list)

    def evaluate_now(self, expr):
        raise NotImplementedError

    def start(self):
        raise NotImplementedError

    def started(self):
        raise NotImplementedError

    def stop(self):
        #Graceful stop
        raise NotImplementedError

    def terminate(self):
        raise NotImplementedError

    def restart(self):
        if self.started:
            self.stop()
        self.start()

    def function(self, expr):
        """Return a `callable` function from a Wolfram Language function `expr`.

        The object returned can be applied on arguments as any other Python function, and
        is evaluated using the underlying Wolfram evaluator.
        """
        def inner(*args, **opts):
            return self.evaluate(WLFunction(expr, *args, **opts))
        return inner

    def __enter__(self):
        if not self.started:
            self.start()
        return self

    def __exit__(self, type, value, traceback):
        if self.started:
            self.stop()

#ASYNC VERSION

class WolframAsyncEvaluator(WolframEvaluatorBase):

    def __init__(self, loop = None):
        if loop:
            self._loop = loop
        else:
            self._loop = asyncio.get_event_loop()

    async def evaluate(self, expr):
        result = await self.evaluate_wrap(expr)
        return await result.get()

    async def evaluate_many(self, expr_list):
        return await wait_all(map(self.evaluate, expr_list), loop = self._loop)

    async def evaluate_wrap(self, expr):
        raise NotImplementedError

    async def start(self):
        raise NotImplementedError

    async def started(self):
        """Indicate that the evaluator is fully started."""
        raise NotImplementedError

    async def stop(self):
        #Graceful stop
        raise NotImplementedError

    async def terminate(self):
        raise NotImplementedError

    async def restart(self):
        if await self.started():
            await self.stop()
        await self.start()

    def function(self, expr):
        async def inner(*args, **opts):
            return await self.evaluate(WLFunction(expr, *args, **opts))
        return inner

    async def __aenter__(self):
        if not await self.started():
            await self.start()
        return self

    async def __aexit__(self, type, value, traceback):
        if await self.started():
            await self.stop()

    def __del__(self, _warnings=warnings):
        super().__del__(_warnings=warnings)
        if self._loop and not self.stopped and not self._loop.is_closed():
            context = {self.__class__.__name__: self,
                       'message': 'Unclosed evaluator.'}
            self._loop.call_exception_handler(context)

class AsyncToSyncWolframEvalutor(WolframEvaluator, WolframAsyncEvaluator):
    def __init__(self):
        super(WolframAsyncEvaluator).__init__(loop=loop)
        self.sync_loop = asyncio.new_event_loop()

    def evaluate(self, expr):
        task = asyncio.create_task(super(WolframAsyncEvaluator).evaluate(*args, **kwargs))
        self._loop.wait(task)
        return self.evaluate_wrap(expr).get()

#     def evaluate_many(self, expr_list):
#         return map(self.evaluate, expr_list)

#     def evaluate_now(self, expr):
#         raise NotImplementedError

    def start(self):
        self.executor = ThreadPoolExecutor(max_workers=self.max_pool_size)
        raise NotImplementedError

#     def started(self):
#         raise NotImplementedError

#     def stop(self):
#         #Graceful stop
#         raise NotImplementedError

#     def terminate(self):
#         raise NotImplementedError

#     def restart(self):
#         if self.started:
#             self.stop()
#         self.start()

#     def function(self, expr):
#         """Return a `callable` function from a Wolfram Language function `expr`.

#         The object returned can be applied on arguments as any other Python function, and
#         is evaluated using the underlying Wolfram evaluator.
#         """
#         def inner(self, *args, **opts):
#             return self.evaluate(WLFunction(expr, *args, **opts))
#         return inner

#     def __enter__(self):
#         if not self.started:
#             self.start()
#         return self

#     def __exit__(self, type, value, traceback):
#         if self.started:
#             self.stop()



# class WolframAsyncCloudKernel(WolframAsyncEvaluator):
#     pass

# class WolframCloudKernel(AsyncToSyncWolframEvalutor, WolframAsyncEvaluator):
#     pass






