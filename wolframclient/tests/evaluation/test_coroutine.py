# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
import unittest

from wolframclient.deserializers import binary_deserialize
from wolframclient.evaluation import (
    WolframCloudAsyncSession, WolframEvaluatorPool,
    WolframLanguageAsyncSession, parallel_evaluate)
from wolframclient.language import wl, wlexpr
from wolframclient.tests.configure import (MSG_JSON_NOT_FOUND, json_config,
                                           secured_authentication_key, server)
from wolframclient.tests.evaluation.test_kernel import \
    TestCaseSettings as TestKernelBase
from wolframclient.utils import six
from wolframclient.utils.api import asyncio, time
from wolframclient.utils.asyncio import get_event_loop, run_in_loop
from wolframclient.utils.tests import TestCase as BaseTestCase

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

LOOP = get_event_loop()


@unittest.skipIf(json_config is None, MSG_JSON_NOT_FOUND)
class TestCoroutineSession(BaseTestCase):
    @classmethod
    def setUpClass(cls):
        cls.KERNEL_PATH = json_config.get('kernel', None)
        cls.setupKernelSession()

    @classmethod
    def tearDownClass(cls):
        cls.tearDownKernelSession()

    @classmethod
    def tearDownKernelSession(cls):
        if cls.async_session is not None:
            LOOP.run_until_complete(cls.async_session.stop())

    @classmethod
    def setupKernelSession(cls):
        cls.async_session = WolframLanguageAsyncSession(
            cls.KERNEL_PATH, kernel_loglevel=logging.INFO)
        cls.async_session.set_parameter('STARTUP_TIMEOUT', 5)
        cls.async_session.set_parameter('TERMINATE_TIMEOUT', 3)
        LOOP.run_until_complete(cls.async_session.start())

    @run_in_loop
    async def test_eval_inputform(self):
        start = time.perf_counter()
        task = asyncio.create_task(
            self.async_session.evaluate(wlexpr('Pause[.1]; Range[3]')))
        timer = time.perf_counter() - start
        self.assertTrue(timer < .1)
        res = await task
        self.assertEqual(res, [1, 2, 3])

    @run_in_loop
    async def test_eval_wlsymbol(self):
        start = time.perf_counter()
        task = asyncio.create_task(
            self.async_session.evaluate(
                wl.CompoundExpression(wl.Pause(.1), wl.Range(2))))
        timer = time.perf_counter() - start
        self.assertTrue(timer < .1)
        res = await task
        self.assertEqual(res, [1, 2])

    @run_in_loop
    async def test_eval_wxf(self):
        start = time.perf_counter()
        task = asyncio.create_task(
            self.async_session.evaluate_wxf(wlexpr('Pause[.1]; Range[3]')))
        timer = time.perf_counter() - start
        self.assertTrue(timer < .1)
        res = await task
        self.assertEqual(binary_deserialize(res), [1, 2, 3])

    @run_in_loop
    async def test_eval_wrap(self):
        start = time.perf_counter()
        task = asyncio.create_task(
            self.async_session.evaluate_wrap(wlexpr('Pause[.1]; Range[3]')))
        timer = time.perf_counter() - start
        self.assertTrue(timer < .1)
        res = await task
        self.assertEqual(res.get(), [1, 2, 3])

    @run_in_loop
    async def test_eval_many(self):
        exprs = [('%s+%s' % (i, i)) for i in range(10)]
        expected = [i + i for i in range(10)]
        res = await self.async_session.evaluate_many(exprs)
        self.assertEqual(res, expected)

    @run_in_loop
    async def test_eval_start(self):
        try:
            async_session = WolframLanguageAsyncSession(
                self.KERNEL_PATH, kernel_loglevel=logging.INFO)
            res = await async_session.evaluate(wl.Plus(1, 1))
            self.assertTrue(res, 2)
        finally:
            if async_session:
                await async_session.terminate()

    @run_in_loop
    async def test_eval_parallel(self):
        tasks = [
            asyncio.create_task(self.async_session.evaluate(i + 1))
            for i in range(10)
        ]
        res = await asyncio.gather(*tasks)
        self.assertEqual(res, list(range(1, 11)))

    def test_kwargs_parameters(self):
        TestKernelBase.class_kwargs_parameters(self,
                                               WolframLanguageAsyncSession)

    def test_bad_kwargs_parameters(self):
        TestKernelBase.class_bad_kwargs_parameters(
            self, WolframLanguageAsyncSession)


@unittest.skipIf(json_config is None, MSG_JSON_NOT_FOUND)
class TestKernelPool(BaseTestCase):

    if json_config:
        KERNEL_PATH = json_config.get('kernel', None)

    @classmethod
    def setUpClass(cls):
        cls.setupKernelSession()

    @classmethod
    def tearDownClass(cls):
        cls.tearDownKernelSession()

    @classmethod
    def tearDownKernelSession(cls):
        if cls.pool is not None:
            LOOP.run_until_complete(cls.pool.terminate())

    @classmethod
    def setupKernelSession(cls):
        cls.pool = WolframEvaluatorPool(
            cls.KERNEL_PATH,
            kernel_loglevel=logging.INFO,
            STARTUP_TIMEOUT=5,
            TERMINATE_TIMEOUT=3)
        LOOP.run_until_complete(cls.pool.start())

    @run_in_loop
    async def test_eval_wlsymbol(self):
        tasks = [
            asyncio.create_task(self.pool.evaluate(wl.FromLetterNumber(i)))
            for i in range(1, 11)
        ]
        res = await asyncio.gather(*tasks)
        self.assertEqual({*res},
                         {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"})

    @run_in_loop
    async def test_eval_inputform(self):
        tasks = [
            asyncio.create_task(
                self.pool.evaluate(wlexpr('FromLetterNumber[%i]' % i)))
            for i in range(1, 11)
        ]
        res = await asyncio.gather(*tasks)
        self.assertEqual({*res},
                         {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"})

    @run_in_loop
    async def test_eval_wxf(self):
        tasks = [
            asyncio.create_task(
                self.pool.evaluate_wxf(wlexpr('FromLetterNumber[%i]' % i)))
            for i in range(1, 11)
        ]
        res = await asyncio.gather(*tasks)
        res = {binary_deserialize(wxf) for wxf in res}
        self.assertEqual(res,
                         {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"})

    @run_in_loop
    async def test_failed_expr(self):
        tasks = [
            asyncio.create_task(self.pool.evaluate(wlexpr('Pause[.1]; 1/0')))
            for i in range(1, 10)
        ]
        res = await asyncio.gather(*tasks)
        self.assertEqual(res, [wl.DirectedInfinity() for _ in range(1, 10)])

    @run_in_loop
    async def test_pool_from_one_kernel(self):
        await self.pool.terminate()
        session = WolframLanguageAsyncSession(self.KERNEL_PATH)
        async with WolframEvaluatorPool(
                session,
                kernel_loglevel=logging.INFO,
                STARTUP_TIMEOUT=5,
                TERMINATE_TIMEOUT=3) as pool:
            await self._pool_evaluation_check(pool)
        self.assertFalse(session.started)
        self.assertTrue(session.stopped)

    @run_in_loop
    async def test_pool_from_one_cloud(self):
        session = WolframCloudAsyncSession(
            credentials=secured_authentication_key, server=server)
        async with WolframEvaluatorPool(
                session,
                kernel_loglevel=logging.INFO,
                STARTUP_TIMEOUT=5,
                TERMINATE_TIMEOUT=3) as pool:
            await self._pool_evaluation_check(pool)
        self.assertFalse(session.started)
        self.assertTrue(session.stopped)

    @run_in_loop
    async def test_pool_from_mixed_kernel_cloud_path(self):
        await self.pool.terminate()
        sessions = (WolframCloudAsyncSession(
            credentials=secured_authentication_key, server=server),
                    WolframLanguageAsyncSession(self.KERNEL_PATH),
                    self.KERNEL_PATH)
        async with WolframEvaluatorPool(
                sessions,
                kernel_loglevel=logging.INFO,
                STARTUP_TIMEOUT=5,
                TERMINATE_TIMEOUT=3) as pool:
            await self._pool_evaluation_check(pool)
        for session in sessions:
            if not isinstance(session, six.string_types):
                self.assertFalse(session.started)
                self.assertTrue(session.stopped)

    async def _pool_evaluation_check(self, pool):
        tasks = [
            asyncio.create_task(pool.evaluate(wl.FromLetterNumber(i)))
            for i in range(1, 11)
        ]
        res = await asyncio.gather(*tasks)
        self.assertEqual({*res},
                         {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"})


@unittest.skipIf(json_config is None, MSG_JSON_NOT_FOUND)
class TestParalleleEvaluate(BaseTestCase):

    if json_config:
        KERNEL_PATH = json_config.get('kernel', None)

    def test_parallel_evaluate_local(self):
        exprs = [wl.FromLetterNumber(i) for i in range(1, 11)]
        res = parallel_evaluate(exprs, evaluator_spec=self.KERNEL_PATH)
        self.assertEqual(res,
                         ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"])

    def test_parallel_evaluate_sizeone(self):
        exprs = [wl.FromLetterNumber(i) for i in range(1, 11)]
        res = parallel_evaluate(
            exprs, evaluator_spec=self.KERNEL_PATH, max_evaluators=1)
        self.assertEqual(res,
                         ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"])

    def test_parallel_evaluate_sizeone(self):
        exprs = [wl.FromLetterNumber(i) for i in range(1, 11)]
        res = parallel_evaluate(
            exprs,
            evaluator_spec=[
                self.KERNEL_PATH, self.KERNEL_PATH, self.KERNEL_PATH
            ],
            max_evaluators=1)
        self.assertEqual(res,
                         ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"])

    def test_parallel_evaluate_cloud(self):
        cloud = WolframCloudAsyncSession(
            credentials=secured_authentication_key, server=server)
        exprs = [wl.FromLetterNumber(i) for i in range(1, 11)]
        res = parallel_evaluate(exprs, evaluator_spec=cloud)
        self.assertEqual(len(res), 10)
        for elem in res:
            self.assertTrue(isinstance(elem, six.string_types))

    def test_parallel_evaluate_mixed(self):
        cloud = WolframCloudAsyncSession(
            credentials=secured_authentication_key, server=server)
        exprs = [wl.FromLetterNumber(i) for i in range(1, 11)]
        res = parallel_evaluate(
            exprs, evaluator_spec=[cloud, self.KERNEL_PATH, cloud])
        self.assertEqual(len(res), 10)
        for elem in res:
            self.assertTrue(isinstance(elem, six.string_types))
