# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
import unittest

from wolframclient.deserializers import binary_deserialize
from wolframclient.evaluation import WolframLanguageSession
from wolframclient.exception import WolframKernelException
from wolframclient.language import wl, wlexpr
from wolframclient.language.expression import WLFunction, WLSymbol
from wolframclient.tests.configure import MSG_JSON_NOT_FOUND, json_config
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.utils.tests import path_to_file_in_data_dir

try:
    import PIL.Image
    has_pil = True
except ImportError:
    has_pil = False

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


@unittest.skipIf(json_config is None, MSG_JSON_NOT_FOUND)
class TestCaseSettings(BaseTestCase):

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
        if cls.kernel_session is not None:
            cls.kernel_session.terminate()

    @classmethod
    def setupKernelSession(cls):
        cls.kernel_session = WolframLanguageSession(
            cls.KERNEL_PATH, kernel_loglevel=logging.INFO)
        cls.kernel_session.set_parameter('STARTUP_TIMEOUT', 5)
        cls.kernel_session.set_parameter('TERMINATE_TIMEOUT', 3)
        cls.kernel_session.start()

    @classmethod
    def class_kwargs_parameters(cls, testcase, kernelclass):
        kernel_session = kernelclass(
            cls.KERNEL_PATH,
            kernel_loglevel=logging.INFO,
            STARTUP_TIMEOUT=5,
            TERMINATE_TIMEOUT=3,
            HIDE_SUBPROCESS_WINDOW=False)
        testcase.assertEqual(
            kernel_session.get_parameter('STARTUP_TIMEOUT'), 5)
        testcase.assertEqual(
            kernel_session.get_parameter('TERMINATE_TIMEOUT'), 3)
        testcase.assertEqual(
            kernel_session.get_parameter('HIDE_SUBPROCESS_WINDOW'), False)

    @classmethod
    def class_bad_kwargs_parameters(cls, testcase, kernelclass):
        with testcase.assertRaises(KeyError):
            kernel_session = kernelclass(
                cls.KERNEL_PATH, kernel_loglevel=logging.INFO, foo=1)
            kernel_session.get_parameter('foo')


@unittest.skipIf(json_config is None, MSG_JSON_NOT_FOUND)
class TestCase(TestCaseSettings):
    def test_evaluate_basic_inputform(self):
        res = self.kernel_session.evaluate('1+1')
        self.assertEqual(res, 2)

    def test_evaluate_basic_wl(self):
        res = self.kernel_session.evaluate(wl.Plus(1, 2))
        self.assertEqual(res, 3)

    def test_evaluate_option(self):
        res = self.kernel_session.evaluate(
            wl.BinarySerialize(1, PerformanceGoal="Size"))
        self.assertEqual(res, b'8C:x\x9csf\x04\x00\x00\x89\x00E')

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
                             ['Infinite expression Power[0, -1] encountered.'])

    def test_silenced_msg(self):
        off = self.kernel_session.evaluate('Off[Power::infy]')
        self.assertEqual(off, None)
        res = self.kernel_session.evaluate_wrap('1/0')
        self.assertEqual(res.get(), WLFunction(WLSymbol(b'DirectedInfinity')))
        self.assertTrue(res.success)
        on = self.kernel_session.evaluate('On[Power::infy]')
        self.assertEqual(on, None)

    def test_one_eval_many_msg(self):
        res = self.kernel_session.evaluate('ImportString["[1,2", "RawJSON"]')
        self.assertEqual(res, WLSymbol('$Failed'))

    def test_one_eval_many_msg_wrap(self):
        res = self.kernel_session.evaluate_wrap(
            'ImportString["[1,2", "RawJSON"]')
        self.assertFalse(res.success)
        expected_msgs = [
            (wl.MessageName(wl.Import, 'jsonarraymissingsep'),
             'Expecting end of array or a value separator.'),
            (wl.MessageName(wl.Import, 'jsonhintposandchar'),
             "An error occurred near character 'EOF', at line 1:6")
        ]
        self.assertListEqual(list(res.iter_messages_tuple()), expected_msgs)

    def test_many_failures(self):
        res = self.kernel_session.evaluate(
            'ImportString["[1,2", "RawJSON"]; 1/0')
        self.assertEqual(res, WLFunction(WLSymbol(b'DirectedInfinity')))

    def test_many_failures_wrap(self):
        res = self.kernel_session.evaluate_wrap(
            'ImportString["[1,2", "RawJSON"]; 1/0')
        self.assertFalse(res.success)
        expected_msgs = [
            'Expecting end of array or a value separator.',
            "An error occurred near character 'EOF', at line 1:6",
            'Infinite expression Power[0, -1] encountered.'
        ]
        expected_msgs_name = [
            wl.MessageName(wl.Import, 'jsonarraymissingsep'),
            wl.MessageName(wl.Import, 'jsonhintposandchar'),
            wl.MessageName(wl.Power, 'infy')
        ]

        expected_tuples = list(zip(expected_msgs_name, expected_msgs))

        self.assertListEqual(res.messages, expected_msgs)
        self.assertListEqual(list(res.iter_messages()), expected_msgs)

        self.assertListEqual(res.messages_name, expected_msgs_name)
        self.assertListEqual(
            list(res.iter_messages_name()), expected_msgs_name)

        self.assertListEqual(list(res.iter_messages_tuple()), expected_tuples)

    def test_valid_evaluate_wxf(self):
        wxf = self.kernel_session.evaluate_wxf('Range[3]')
        result = binary_deserialize(wxf)
        self.assertEqual(result, [1, 2, 3])

    def test_err_evaluate_wxf(self):
        wxf = self.kernel_session.evaluate_wxf('Range[3')
        result = binary_deserialize(wxf)
        self.assertEqual(result, WLSymbol('$Failed'))

    def test_auto_start_session(self):
        try:
            session = WolframLanguageSession(self.KERNEL_PATH)
            res = session.evaluate('1+1')
            self.assertEqual(res, 2)
        except Exception as e:
            logger.exception(e)
        finally:
            session.terminate()
            self.assertTrue(session.stopped)

    def test_pure_function_inputform(self):
        f = self.kernel_session.function('#+1&')
        self.assertEqual(f(3), 4)
        self.assertEqual(f(10), 11)

    def test_function_inputform(self):
        stringQ = self.kernel_session.function('AllTrue[{##}, StringQ] &')
        self.assertTrue(stringQ('abc'))
        self.assertTrue(stringQ('a', 'b', 'c'))
        self.assertFalse(stringQ(1))
        self.assertFalse(stringQ('a', 1))

    def test_function_symbolic(self):
        total_range = self.kernel_session.function(
            wl.Composition(wl.Total, wl.Range))
        self.assertEqual(total_range(5), 15)

    def test_wlexpr_wrapper(self):
        res = self.kernel_session.evaluate(wl.Map(wlexpr('#+1&'), [1, 2, 3]))
        self.assertEqual(res, [2, 3, 4])

    def test_built_in_symbols(self):
        self.assertEqual(self.kernel_session.evaluate(wl.Null), None)
        self.assertEqual(self.kernel_session.evaluate(None), None)
        self.assertEqual(
            self.kernel_session.evaluate(wlexpr('None')), WLSymbol('None'))
        self.assertEqual(self.kernel_session.evaluate(wlexpr('True')), True)
        self.assertEqual(self.kernel_session.evaluate(True), True)
        self.assertEqual(self.kernel_session.evaluate(wlexpr('False')), False)
        self.assertEqual(self.kernel_session.evaluate(False), False)
        self.assertEqual(self.kernel_session.evaluate(wl.StringQ('foo')), True)

    def test_eval_many(self):
        exprs = [('%s+%s' % (i, i)) for i in range(10)]
        expected = [i + i for i in range(10)]
        res = self.kernel_session.evaluate_many(exprs)
        self.assertEqual(res, expected)

    def test_built_in_symbols_as_func(self):
        func_null = self.kernel_session.function('Null')
        res = func_null(5)
        self.assertEqual(res, WLFunction(None, 5))

    def test_evaluate_global_func(self):
        self.kernel_session.evaluate(
            'ClearAll[f]; f[x_String]:=StringReverse[x]')
        inv = self.kernel_session.function(wl.Global.f)
        self.assertEqual(inv('abc'), 'cba')

    def test_kwargs_parameters(self):
        TestCaseSettings.class_kwargs_parameters(self, WolframLanguageSession)

    def test_bad_kwargs_parameters(self):
        self.class_bad_kwargs_parameters(self, WolframLanguageSession)

    IMAGE_FILES_DIMS = {
        "10ct_32bit_128.tiff": [128, 128],
        "16_bit_binary_pgm.png": [20, 100],
        "hopper.ppm": [128, 128],
        "pal1wb.bmp": [127, 64],
        "pil_sample_cmyk.jpg": [100, 100],
        "umbrellaRGBA.png": [1789, 1920]
    }

    @unittest.skipIf(not has_pil, "PIL not found skipping image test.")
    def test_images_serialization(self):
        for path, dimensions in self.IMAGE_FILES_DIMS.items():
            with PIL.Image.open(path_to_file_in_data_dir(path)) as img:
                res = self.kernel_session.evaluate(wl.ImageDimensions(img))
                self.assertEqual(res, dimensions)

    @unittest.skipIf(not has_pil, "PIL not found skipping image test.")
    def test_images_in_expr(self):
        img1_path = "hopper.ppm"
        img2_path = "pal1wb.bmp"
        with PIL.Image.open(path_to_file_in_data_dir(img1_path)) as img1:
            with PIL.Image.open(path_to_file_in_data_dir(img2_path)) as img2:
                res = self.kernel_session.evaluate(
                    wl.Map(wl.ImageDimensions, {
                        'img1': img1,
                        'img2': img2
                    }))
                self.assertEqual(
                    res, {
                        'img1': self.IMAGE_FILES_DIMS[img1_path],
                        'img2': self.IMAGE_FILES_DIMS[img2_path]
                    })

    def test_stop_start_restart_status(self):
        session = None
        try:
            session = WolframLanguageSession(self.KERNEL_PATH)
            self.assertFalse(session.started)
            self.assertTrue(session.stopped)
            session.start()
            self.assertTrue(session.started)
            self.assertFalse(session.stopped)
            session.stop()
            self.assertFalse(session.started)
            self.assertTrue(session.stopped)
            session.restart()
            self.assertTrue(session.started)
            self.assertFalse(session.stopped)
            session.terminate()
            self.assertFalse(session.started)
            self.assertTrue(session.stopped)
        finally:
            if session:
                session.terminate()


class TestSessionTimeout(TestCaseSettings):
    def test_evaluate_async_basic_inputform(self):
        future = self.kernel_session.evaluate_future('1+1')
        self.assertEqual(future.result(timeout=1), 2)

    def test_evaluate_async_basic_wl(self):
        future = self.kernel_session.evaluate_future(wl.Plus(1, 2))
        self.assertEqual(future.result(timeout=1), 3)

    def test_evaluate_multiple_async(self):
        with WolframLanguageSession(self.KERNEL_PATH) as kernel_session:
            future1 = kernel_session.evaluate_future('3+4')
            result1 = future1.result(timeout=3)
            self.assertEqual(result1, 7)
            future2 = kernel_session.evaluate_future('10+1')
            self.assertEqual(future2.result(timeout=1), 11)
            future3 = kernel_session.evaluate_future('100+1')
            self.assertEqual(future3.result(timeout=1), 101)

    def test_many_failures_wrap_async(self):
        future = self.kernel_session.evaluate_wrap_future(
            'ImportString["[1,2", "RawJSON"]; 1/0')
        res = future.result(timeout=1)
        self.assertFalse(res.success)
        expected_msgs = [
            'Expecting end of array or a value separator.',
            "An error occurred near character 'EOF', at line 1:6",
            'Infinite expression Power[0, -1] encountered.'
        ]
        expected_msgs_name = [
            wl.MessageName(wl.Import, 'jsonarraymissingsep'),
            wl.MessageName(wl.Import, 'jsonhintposandchar'),
            wl.MessageName(wl.Power, 'infy')
        ]
        expected_tuples = list(zip(expected_msgs_name, expected_msgs))

        self.assertListEqual(res.messages, expected_msgs)
        self.assertListEqual(list(res.iter_messages()), expected_msgs)

        self.assertListEqual(res.messages_name, expected_msgs_name)
        self.assertListEqual(
            list(res.iter_messages_name()), expected_msgs_name)

        self.assertListEqual(list(res.iter_messages_tuple()), expected_tuples)

    def test_valid_evaluate_wxf_async(self):
        future = self.kernel_session.evaluate_wxf_future('Range[3]')
        wxf = future.result(timeout=1)
        result = binary_deserialize(wxf)
        self.assertEqual(result, [1, 2, 3])


@unittest.skipIf(json_config is None, MSG_JSON_NOT_FOUND)
class TestCaseSession(TestCaseSettings):
    def test_kernel_init_bad_path(self):
        with self.assertRaises(WolframKernelException):
            WolframLanguageSession('path/that/does/not/exists')

    def test_kernel_init_nonstring_path(self):
        with self.assertRaises(ValueError):
            WolframLanguageSession(False)

    def test_terminated_session_autorestart(self):
        session = None
        try:
            session = WolframLanguageSession(self.KERNEL_PATH)
            session.start()
            session.stop()
            res = session.evaluate('1+1')
            self.assertEqual(res, 2)
        finally:
            if session:
                session.terminate()


@unittest.skipIf(json_config is None, MSG_JSON_NOT_FOUND)
class TestCaseInternalFunctions(TestCaseSettings):
    def test_default_loglevel(self):
        with WolframLanguageSession(self.KERNEL_PATH) as session:
            res = session.evaluate(
                'ClientLibrary`Private`$LogLevel == Infinity')
            self.assertTrue(res)
            # This is not possible. Logging was not enabled in the first place.
            session.evaluate('ClientLibrary`SetInfoLogLevel[]')
            # Log level remains to NOTSET
            res = session.evaluate(
                'ClientLibrary`Private`$LogLevel == ClientLibrary`Private`$NOTSET'
            )
            logger.info('LOG LEVEL: %s',
                        session.evaluate('ClientLibrary`Private`$LogLevel'))
            self.assertTrue(res)

    def test_set_loglevel(self):
        with WolframLanguageSession(
                self.KERNEL_PATH, kernel_loglevel=logging.WARN) as session:
            res = session.evaluate(
                'ClientLibrary`Private`$LogLevel == ClientLibrary`Private`$WARN'
            )
            self.assertTrue(res)
