# -*- coding: utf-8 -*-
from __future__ import absolute_import, print_function, unicode_literals
import logging
import unittest
import json
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.utils import six
if not six.JYTHON:
    from wolframclient.evaluation import WolframLanguageSession, SecuredAuthenticationKey, UserIDPassword, WolframCloudSession
logging.basicConfig(filename='/tmp/python_testsuites.log',
                    filemode='a',
                    format='%(asctime)s, %(name)s %(levelname)s %(message)s',
                    level=logging.INFO)
logger = logging.getLogger(__name__)


# @unittest.skipIf(six.JYTHON, "Not supported in Jython.")
# class TestCase(BaseTestCase):

#     KERNEL_PATH = '/Applications/Mathematica.app/Contents/MacOS/WolframKernel'

#     user_config_file = '/private/etc/user_credentials.json'
#     api_owner = 'dorianb'

#     def setUp(self):
#         self.kernel_session = WolframLanguageSession(TestCase.KERNEL_PATH, log_kernel=False)
#         self.kernel_session.start()
#         with open(TestCase.user_config_file, 'r') as fp:
#             json_user_config = json.load(fp)
#             self.sak = SecuredAuthenticationKey(
#                 json_user_config['SAK']['consumer_key'],
#                 json_user_config['SAK']['consumer_secret']
#             )
#             self.user_cred = UserIDPassword(
#                 json_user_config['User']['id'],
#                 json_user_config['User']['password']
#             )
#         self.cloudsession = WolframCloudSession(authentication=self.sak)

#     def tearDown(self):
#         if self.kernel_session is not None:
#             self.kernel_session.terminate()
#         if self.cloudsession is not None:
#             self.cloudsession.terminate()

#     def test_call_api(self):
#         input = '1+1'
#         call = WolframCall()