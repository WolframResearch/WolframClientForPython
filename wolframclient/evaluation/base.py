# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.async import wait_all, syncronous_wait_all
from wolframclient.language.expression import WLFunction
from wolframclient.utils.functional import first

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


#SYNC VERSION

class WolframEvaluator(WolframEvaluatorMixin):

    def started(self):
        raise NotImplementedError

    def evaluate(self, expr):
        return self.evaluate_wrap(expr).get()

    def evaluate_many(self, expr_list):
        return map(self.evaluate, expr_list)

    def evaluate_now(self, expr):
        raise NotImplementedError

    def start(self):
        raise NotImplementedError

    def stop(self):
        #Graceful stop
        raise NotImplementedError

    def terminate(self):
        raise NotImplementedError

    def restart(self):
        if self.started():
            self.stop()
        self.start()

    def function(self, expr):
        def inner(self, *args, **opts):
            return self.evaluate(WLFunction(expr, *args, **opts))
        return inner

    def __enter__(self):
        if not self.started():
            self.start()
        return self

    def __exit__(self, type, value, traceback):
        if self.started():
            self.stop()



#ASYNC VERSION

class WolframAsyncEvaluator(WolframEvaluatorMixin):

    def __init__(self, loop = None):
        self.loop = loop

    async def started(self):
        raise NotImplementedError

    async def evaluate(self, expr):
        return await self.evaluate_wrap(expr).get()

    async def evaluate_many(self, expr_list):
        return await wait_all(map(self.evaluate, expr_list), loop = self.loop)

    async def evaluate_wrap(self, expr):
        raise NotImplementedError

    async def start(self):
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
        async def inner(self, *args, **opts):
            return await self.evaluate(WLFunction(expr, *args, **opts))
        return inner

    def __aenter__(self):
        if not await self.started():
            await self.start()
        return self

    def __aexit__(self, type, value, traceback):
        if await self.started():
            await self.stop()

"""
class AsyncToSyncWolframEvalutor:

    def started(self):
        return first(syncronous_wait_all(super().started()))


class WolframAsyncCloudKernel(WolframAsyncEvaluator):
    pass

class WolframCloudKernel(AsyncToSyncWolframEvalutor, WolframAsyncEvaluator):
    pass
"""





