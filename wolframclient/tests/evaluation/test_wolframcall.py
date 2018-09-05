# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
if not six.JYTHON:
    from wolframclient.evaluation import WolframCall, WolframAPICall
from wolframclient.language import wl
from wolframclient.language.expression import WLSymbol
from wolframclient.exception import WolframEvaluationException
from wolframclient.logger.utils import setup_logging_to_file
from wolframclient.utils.encoding import force_text
from wolframclient.tests.evaluation.test_cloud import TestCaseSettings as SessionTestCase
from wolframclient.tests.evaluation.test_kernel import TestCaseSettings as KernelTestCase
import logging
import unittest
import json

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


@unittest.skipIf(six.JYTHON, "Not supported in Jython.")
class TestCase(SessionTestCase, KernelTestCase):

    @classmethod
    def setUpClass(cls):
        cls.setupCloudSession()
        cls.setupKernelSession()

    @classmethod
    def tearDownClass(cls):
        cls.tearDownKernelSession()
        cls.tearDownCloudSession()

    @unittest.skipIf(six.JYTHON, "Not supported in Jython.")
    def test_wolfram_call_kernel(self):
        result = WolframCall(self.kernel_session, '1+1').perform()
        self.assertEqual(result, 2)

    @unittest.skipIf(six.JYTHON, "Not supported in Jython.")
    def test_wolfram_call_kernel_fail(self):
        result = WolframCall(self.kernel_session, 'Range[3').perform()
        self.assertEqual(result, WLSymbol('$Failed'))

    @unittest.skipIf(six.JYTHON, "Not supported in Jython.")
    def test_wolfram_call_kernel_wrap_fail(self):
        result = WolframCall(self.kernel_session, 'Range[3').perform_wrap()
        self.assertFalse(result.success)
        self.assertEqual(result.get(), WLSymbol('$Failed'))

    def test_call_cloud_evaluation(self):
        result = WolframCall(self.cloud_session, '1+1').perform()
        self.assertEqual(result, '2')

    def test_call_cloud_evaluation_fail(self):
        result = WolframCall(self.cloud_session, 'Range[3').perform()
        self.assertEqual(result, 'Null')
    
    def test_call_cloud_evaluation_fail_wrap(self):
        res = WolframCall(self.cloud_session, 'Range[3').perform_wrap()
        self.assertFalse(res.success)
        self.assertEqual(res.get(), 'Null')
        self.assertTrue(len(res.failure) > 0)

    def test_wolfram_api_call_image(self):
        api = (self.api_owner, 'api/private/imagedimensions')
        apicall = WolframAPICall(self.cloud_session, api)
        with open(self.get_data_path('32x2.png'), 'rb') as fp:
            apicall.add_file_parameter('image', fp)
            res = apicall.perform()
            self.assertTrue(res.success)
            res = json.loads(res.get())
            self.assertListEqual(res, [32, 2])

    def test_wolfram_api_call_str(self):
        api = (self.api_owner, 'api/private/stringreverse')
        apicall = WolframAPICall(self.cloud_session, api)
        apicall.add_parameter('str', 'abcde')
        res = apicall.perform().get()
        self.assertEqual('"edcba"', force_text(res))
