# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers import export
from wolframclient.deserializers import binary_deserialize
from wolframclient.language import wl
from wolframclient.exception import WolframEvaluationException, WolframKernelException
from wolframclient.language.expression import WLSymbol, WLFunction
from wolframclient.logger.utils import setup_logging_to_file
from wolframclient.utils import six
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.tests import json_config

import logging
import unittest

if not six.JYTHON:
    from wolframclient.evaluation import WolframLanguageSession, WolframLanguageAsyncSession

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

@unittest.skipIf(json_config is None, "Could not find configuration file as specified in wolframclient/tests/local_config_sample.json")
@unittest.skipIf(six.JYTHON, "Not supported in Jython.")
class TestCaseSettings(BaseTestCase):

    @classmethod
    def setUpClass(cls):
        cls.KERNEL_PATH = json_config['kernel']
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

@unittest.skipIf(json_config is None, "Could not find configuration file as specified in wolframclient/tests/local_config_sample.json")
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
        res = self.kernel_session.evaluate('Range[5')
        self.assertEqual(res, WLSymbol('$Failed'))

    def test_malformed_expr_wrap(self):
        res = self.kernel_session.evaluate_wrap('Range[5')
        self.assertFalse(res.success)
        self.assertEqual(res.get(), WLSymbol('$Failed'))

    def test_one_msg(self):
        res = self.kernel_session.evaluate('1/0')
        self.assertEqual(res, WLFunction(WLSymbol(b'DirectedInfinity')))

    def test_one_msg_wrap(self):
        res = self.kernel_session.evaluate_wrap('1/0')
        self.assertFalse(res.success)
        self.assertListEqual(res.messages,
            [('Power::infy', 'Infinite expression Infinity encountered.')]
        )

    def test_silenced_msg(self):
        off = self.kernel_session.evaluate('Off[Power::infy]')
        self.assertEqual(off, wl.Null)
        res = self.kernel_session.evaluate_wrap('1/0')
        self.assertEqual(res.get(), WLFunction(WLSymbol(b'DirectedInfinity')))
        self.assertTrue(res.success)
        on = self.kernel_session.evaluate('On[Power::infy]')
        self.assertEqual(on, wl.Null)

    def test_one_eval_many_msg(self):
        res = self.kernel_session.evaluate('ImportString["[1,2", "RawJSON"]')
        self.assertEqual(res, WLSymbol('$Failed'))
        
    def test_one_eval_many_msg_wrap(self):
        res = self.kernel_session.evaluate_wrap('ImportString["[1,2", "RawJSON"]')
        self.assertFalse(res.success)
        expected_msgs = [('Import::jsonarraymissingsep', 'Expecting end of array or a value separator.'), 
        ('Import::jsonhintposandchar', "An error occurred near character 'EOF', at line 1:6")]
        self.assertListEqual(res.messages, expected_msgs)

    def test_many_failures(self):
        res = self.kernel_session.evaluate('ImportString["[1,2", "RawJSON"]; 1/0')
        self.assertEqual(res, WLFunction(WLSymbol(b'DirectedInfinity')))
    
    def test_many_failures_wrap(self):
        res = self.kernel_session.evaluate_wrap('ImportString["[1,2", "RawJSON"]; 1/0')
        self.assertFalse(res.success)
        expected_msgs = [('Import::jsonarraymissingsep', 'Expecting end of array or a value separator.'), 
        ('Import::jsonhintposandchar', "An error occurred near character 'EOF', at line 1:6"), 
        ('Power::infy', 'Infinite expression Infinity encountered.')]
        self.assertListEqual(res.messages, expected_msgs)

    def test_valid_evaluate_wxf(self):
        wxf = self.kernel_session.evaluate_wxf('Range[3]')
        result = binary_deserialize(wxf)
        self.assertEqual(result, [1,2,3])

    def test_err_evaluate_wxf(self):
        wxf = self.kernel_session.evaluate_wxf('Range[3')
        result = binary_deserialize(wxf)
        self.assertEqual(result, WLSymbol('$Failed'))

    @unittest.skipIf(six.PY2, "No async call on Python2.")
    def test_evaluate_async(self):
        with WolframLanguageAsyncSession(self.KERNEL_PATH) as async_session:
            future1 = async_session.evaluate('3+4')
            result1 = future1.result(timeout=3)
            self.assertEqual(result1, 7)
            future2 = async_session.evaluate('10+1')
            self.assertEqual(future2.result(timeout=1), 11)
            future3 = async_session.evaluate('100+1')
            self.assertEqual(future3.result(timeout=1), 101)


@unittest.skipIf(json_config is None, "Could not find configuration file as specified in wolframclient/tests/local_config_sample.json")
class TestCaseSession(TestCaseSettings):
    def test_kernel_init_bad_path(self):
        with self.assertRaises(WolframKernelException):
            WolframLanguageSession('path/that/does/not/exists')

    def test_kernel_init_nonstring_path(self):
        with self.assertRaises(ValueError):
            WolframLanguageSession(None)

    def test_non_started_session(self):
        session = WolframLanguageSession(self.KERNEL_PATH)
        with self.assertRaises(WolframKernelException):
            session.evaluate('1+1')

    def test_terminated_session(self):
        session = WolframLanguageSession(self.KERNEL_PATH)
        session.start()
        session.terminate()
        with self.assertRaises(WolframKernelException):
            session.evaluate('1+1')


@unittest.skipIf(json_config is None, "Could not find configuration file as specified in wolframclient/tests/local_config_sample.json")
class TestCaseInternalFunctions(TestCaseSettings):
    def test_default_loglevel(self):
        with WolframLanguageSession(self.KERNEL_PATH) as session:
            res = session.evaluate('ClientLibrary`Private`$LogLevel == Infinity')
            self.assertEqual(res, WLSymbol('True'))
            # This is not possible. Logging was not enabled in the first place.
            session.evaluate('ClientLibrary`SetInfoLogLevel[]`')
            # Log level remains to NOTSET
            res = session.evaluate(
                'ClientLibrary`Private`$LogLevel == ClientLibrary`Private`$NOTSET')
            self.assertEqual(res, WLSymbol('True'))

    def test_set_loglevel(self):
        with WolframLanguageSession(self.KERNEL_PATH, kernel_loglevel=logging.WARN) as session:
            res = session.evaluate(
                'ClientLibrary`Private`$LogLevel == ClientLibrary`Private`$WARN')
            self.assertEqual(res, WLSymbol('True'))
