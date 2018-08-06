# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language import wl
from wolframclient.language.expression import WLSymbol
from wolframclient.logger.utils import setup_logging_to_file
from wolframclient.utils import six
from wolframclient.utils.tests import TestCase as BaseTestCase

import logging
import unittest

if not six.JYTHON:
    from wolframclient.evaluation import WolframLanguageSession

setup_logging_to_file('/tmp/python_testsuites.log', level=logging.DEBUG)
logger = logging.getLogger(__name__)

@unittest.skipIf(six.JYTHON, "Not supported in Jython.")
class TestCaseSettings(BaseTestCase):
    KERNEL_PATH = '/Applications/Wolfram Desktop 2.app/Contents/MacOS/WolframKernel'

    @classmethod
    def setUpClass(cls):
        cls.setupKernelSession()

    @classmethod
    def tearDownClass(cls):
        cls.tearDownKernelSession()

    @classmethod
    def tearDownKernelSession(cls):
        if cls.kernel_session is not None:
            cls.kernel_session.terminate()

    @classmethod
    def setupKernelSession(cls):
        cls.kernel_session = WolframLanguageSession(
            cls.KERNEL_PATH, log_kernel=False)
        cls.kernel_session.set_parameter('STARTUP_READ_TIMEOUT', 2)
        cls.kernel_session.set_parameter('TERMINATE_READ_TIMEOUT', 3)
        cls.kernel_session.start()

class TestCase(TestCaseSettings):
    def test_evaluate_basic_inputform(self):
        res = self.kernel_session.evaluate('1+1')
        self.assertEqual(res.get(), 2)

    def test_evaluate_basic_wl(self):
        res = self.kernel_session.evaluate(wl.Plus(1, 2))
        self.assertEqual(res.get(), 3)

    def test_evaluate_variable_updates(self):
        self.kernel_session.evaluate('ClearAll[x]; x=1')
        self.kernel_session.evaluate('x++')
        res = self.kernel_session.evaluate('x+=10')
        self.assertEqual(res.get(), 12)

    def test_evaluate_variable_context(self):
        self.kernel_session.evaluate('ClearAll[x]; x[] := foo')
        res = self.kernel_session.evaluate('Context[x]')
        self.assertEqual(res.get(), 'Global`')
        res = self.kernel_session.evaluate('Context[info]')
        self.assertEqual(res.get(), 'Global`')

    def test_malformed_expr(self):
        res = self.kernel_session.evaluate('Range[5')
        self.assertFalse(res.success)
        self.assertEqual(res.get(), WLSymbol('$Failed'))

    def test_one_msg(self):
        res = self.kernel_session.evaluate(
            '1/0')
        self.assertFalse(res.success)
        self.assertListEqual(res.messages,
            ['Infinite expression Infinity encountered.']
        )

    def test_silenced_msg(self):
        off = self.kernel_session.evaluate('Off[Power::infy]')
        self.assertTrue(off.success)
        res = self.kernel_session.evaluate('1/0')
        self.assertTrue(res.success)
        on = self.kernel_session.evaluate('On[Power::infy]')
        self.assertTrue(on.success)
        self.assertEqual(res.get(), WLSymbol('ComplexInfinity'))

    def test_one_eval_many_msg(self):
        res = self.kernel_session.evaluate(
            'ImportString["[1,2", "RawJSON"]')
        self.assertFalse(res.success)
        expected_msgs = [
            'Expecting end of array or a value separator.',
            "An error occurred near character 'EOF', at line 1:6"]
        self.assertListEqual(res.messages, expected_msgs)

    def test_many_failures(self):
        res = self.kernel_session.evaluate('ImportString["[1,2", "RawJSON"]; 1/0')
        self.assertFalse(res.success)
        expected_msgs = [
            'Expecting end of array or a value separator.',
            "An error occurred near character 'EOF', at line 1:6",
            'Infinite expression Infinity encountered.']
        self.assertListEqual(res.messages, expected_msgs)

    @unittest.skipIf(six.PY2, "No async call on Python2.")
    def test_evaluate_async(self):
        future1 = self.kernel_session.evaluate_async('3+4')
        future2 = self.kernel_session.evaluate_async('10+1')
        future3 = self.kernel_session.evaluate_async('100+1')
        self.assertEqual(future1.result().get(), 7)
        self.assertEqual(future2.result().get(), 11)
        self.assertEqual(future3.result().get(), 101)