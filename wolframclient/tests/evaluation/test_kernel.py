# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers import export
from wolframclient.language import wl
from wolframclient.exception import WolframEvaluationException
from wolframclient.language.expression import WLSymbol, WLFunction
from wolframclient.logger.utils import setup_logging_to_file
from wolframclient.utils import six
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.tests import json_config

import logging
import unittest

if not six.JYTHON:
    from wolframclient.evaluation import WolframLanguageSession

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

@unittest.skipIf(json_config is None, "Could not find configuration file wolframclient/tests/local_config.json")
@unittest.skipIf(six.JYTHON, "Not supported in Jython.")
class TestCaseSettings(BaseTestCase):
    KERNEL_PATH = json_config['kernel']

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
            cls.KERNEL_PATH, kernel_loglevel=logging.INFO)
        cls.kernel_session.set_parameter('STARTUP_READ_TIMEOUT', 5)
        cls.kernel_session.set_parameter('TERMINATE_READ_TIMEOUT', 3)
        cls.kernel_session.start()

class TestCase(TestCaseSettings):
    def test_evaluate_basic_inputform(self):
        res = self.kernel_session.evaluate('1+1')
        self.assertEqual(res, 2)

    def test_evaluate_basic_wl(self):
        res = self.kernel_session.evaluate(wl.Plus(1, 2))
        self.assertEqual(res, 3)

    def test_evaluate_wxf_inputform(self):
        wxf = export(wl.MinMax([1, -2, 3, 5]), target_format='wxf')
        res = self.kernel_session.evaluate(wxf)
        self.assertEqual(res, [-2, 5])

    def test_attr_call_function_no_arg(self):
        res = self.kernel_session.List()
        self.assertListEqual(res, [])

    def test_attr_call_function_with_1arg(self):
        res = self.kernel_session.MinMax([-1, 2, 5])
        self.assertListEqual(res, [-1, 5])

    def test_attr_call_function_with_many_args(self):
        res = self.kernel_session.Part([[1, 2, 3], [4, 5, 6]], -1, 1)
        self.assertEqual(res, 4)

    def test_evaluate_variable_updates(self):
        self.kernel_session.evaluate('ClearAll[x]; x=1')
        self.kernel_session.evaluate('x++')
        res = self.kernel_session.evaluate('x+=10')
        self.assertEqual(res, 12)

    def test_evaluate_variable_context(self):
        self.kernel_session.evaluate('ClearAll[x]; x[] := foo')
        res = self.kernel_session.evaluate('Context[x]')
        self.assertEqual(res, 'Global`')
        res = self.kernel_session.evaluate('Context[info]')
        self.assertEqual(res, 'Global`')

    def test_malformed_expr(self):
        with self.assertRaises(WolframEvaluationException):
            res = self.kernel_session.evaluate('Range[5')
    
    def test_malformed_expr_wrap(self):
        res = self.kernel_session.evaluate_wrap('Range[5')
        self.assertFalse(res.success)
        self.assertEqual(res.get(), WLSymbol('$Failed'))

    def test_one_msg(self):
        with self.assertRaises(WolframEvaluationException):
            res = self.kernel_session.evaluate('1/0')
    
    def test_one_msg_wrap(self):
        res = self.kernel_session.evaluate_wrap('1/0')
        self.assertFalse(res.success)
        self.assertListEqual(res.messages,
            ['Infinite expression Infinity encountered.']
        )

    def test_silenced_msg(self):
        off = self.kernel_session.evaluate('Off[Power::infy]')
        self.assertEqual(off, wl.Null)
        res = self.kernel_session.evaluate('1/0')
        self.assertEqual(res, WLFunction(WLSymbol(b'DirectedInfinity')))
        on = self.kernel_session.evaluate('On[Power::infy]')
        self.assertEqual(on, wl.Null)

    def test_one_eval_many_msg(self):
        with self.assertRaises(WolframEvaluationException):
            res = self.kernel_session.evaluate('ImportString["[1,2", "RawJSON"]')
        
    def test_one_eval_many_msg_wrap(self):
        res = self.kernel_session.evaluate_wrap('ImportString["[1,2", "RawJSON"]')
        self.assertFalse(res.success)
        expected_msgs = [
            'Expecting end of array or a value separator.',
            "An error occurred near character 'EOF', at line 1:6"]
        self.assertListEqual(res.messages, expected_msgs)

    def test_many_failures(self):
        with self.assertRaises(WolframEvaluationException):
            res = self.kernel_session.evaluate('ImportString["[1,2", "RawJSON"]; 1/0')
    
    def test_many_failures_wrap(self):
        res = self.kernel_session.evaluate_wrap('ImportString["[1,2", "RawJSON"]; 1/0')
        self.assertFalse(res.success)
        expected_msgs = [
            'Expecting end of array or a value separator.',
            "An error occurred near character 'EOF', at line 1:6",
            'Infinite expression Infinity encountered.']
        self.assertListEqual(res.messages, expected_msgs)

    @unittest.skipIf(six.PY2, "No async call on Python2.")
    def test_evaluate_async(self):
        future1 = self.kernel_session.evaluate_async('3+4')
        result1 = future1.result(timeout=3)
        self.assertEqual(result1, 7)
        future2 = self.kernel_session.evaluate_async('10+1')
        self.assertEqual(future2.result(timeout=1), 11)
        future3 = self.kernel_session.evaluate_async('100+1')
        self.assertEqual(future3.result(timeout=1), 101)
