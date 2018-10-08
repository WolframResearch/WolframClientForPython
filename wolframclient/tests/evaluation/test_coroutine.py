# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
import unittest
from functools import wraps
from wolframclient.tests.configure import MSG_JSON_NOT_FOUND, json_config
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.utils.api import time, asyncio
from wolframclient.evaluation import WolframLanguageAsyncSession

def test_coroutine(cor):
    @wraps(cor)
    def wrapped(*args, **kwargs):
        try:
            loop = asyncio.get_running_loop()
        except RuntimeError:
            loop = asyncio.new_event_loop()
        return loop.run_until_complete(cor(*args, **kwargs))
    return wrapped

@unittest.skipIf(json_config is None, MSG_JSON_NOT_FOUND)
class TestCoroutineSession(BaseTestCase):
    @classmethod
    def setUpClass(cls):
        cls.KERNEL_PATH = json_config['kernel']
        cls.setupKernelSession()

    @classmethod
    def tearDownClass(cls):
        cls.tearDownKernelSession()

    @classmethod
    def tearDownKernelSession(cls):
        if cls.async_session is not None:
            cls.async_session.terminate()

    @classmethod
    def setupKernelSession(cls):
        cls.async_session = WolframLanguageAsyncSession(
            cls.KERNEL_PATH, kernel_loglevel=logging.INFO)
        cls.async_session.set_parameter('STARTUP_READ_TIMEOUT', 5)
        cls.async_session.set_parameter('TERMINATE_READ_TIMEOUT', 3)
        cls.async_session.start()
        
    @test_coroutine
    async def test_eval_inputform(self):
        start = time.perf_counter()
        task = asyncio.ensure_task(self.async_session.evaluate('Pause[.1]; Range[3]'))
        timer = time.perf_counter() - start
        self.assertTrue(timer < .1)
        res = await task
        self.assertEqual(res, [1,2,3])
