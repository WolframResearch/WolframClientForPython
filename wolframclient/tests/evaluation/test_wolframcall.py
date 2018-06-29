# -*- coding: utf-8 -*-
from __future__ import absolute_import, print_function, unicode_literals
import logging
import unittest
import json
from wolframclient.logger.utils import setup_logging_to_file
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.utils import six
if not six.JYTHON:
    from wolframclient.evaluation import WolframLanguageSession, SecuredAuthenticationKey, UserIDPassword, WolframCloudSession, WolframCall
from wolframclient.tests.evaluation.test_cloud import TestCaseSettings as SessionTestCase
from wolframclient.tests.evaluation.test_kernel import TestCaseSettings as KernelTestCase
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

    def _call_api(self, session):
        result = WolframCall(session, '1+1').perform()
        self.assertEqual(result.get(), b'2')
        result = WolframCall(session, 'Range[3').perform()
        self.assertEqual(result.get(), b'$Failed')

    def test_call_api_kernel(self):
        self._call_api(self.kernel_session)

    @unittest.skip('Cloud bug in /evaluations')
    def test_call_api_cloud(self):
        self._call_api(self.cloud_session)
