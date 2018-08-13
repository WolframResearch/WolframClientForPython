# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.evaluation import WolframCall
from wolframclient.language.expression import WLSymbol
from wolframclient.logger.utils import setup_logging_to_file
from wolframclient.tests.evaluation.test_cloud import TestCaseSettings as SessionTestCase
from wolframclient.tests.evaluation.test_kernel import TestCaseSettings as KernelTestCase
from wolframclient.utils import six
import logging
import unittest

setup_logging_to_file('/tmp/python_testsuites.log', level=logging.INFO)
logger = logging.getLogger(__name__)


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
    def test_call_api_kernel(self):
        result = WolframCall(self.kernel_session, '1+1').perform()
        self.assertEqual(result.get(), 2)

    @unittest.skipIf(six.JYTHON, "Not supported in Jython.")
    def test_call_api_kernel_fail(self):
        result = WolframCall(self.kernel_session, 'Range[3').perform()
        self.assertEqual(result.get(), WLSymbol('$Failed'))

    def test_call_api_cloud(self):
        result = WolframCall(self.cloud_session, '1+1').perform()
        self.assertEqual(result.get(), '2')

    def test_call_api_cloud_fail(self):
        result = WolframCall(self.cloud_session, 'Range[3').perform()
        self.assertFalse(result.success)
        self.assertEqual(result.result, 'Null')
