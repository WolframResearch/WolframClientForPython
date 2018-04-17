from __future__ import absolute_import, print_function, unicode_literals

import unittest

from wolframclient.evaluation.cloud.cloudsession import URLBuilder, WolframCloudSession
from wolframclient.evaluation.cloud.oauth import SecuredAuthenticationKey, UserCredentials
from wolframclient.evaluation.configuration import sak_configuration, user_credential_configuration
from wolframclient.utils.six import string_types
from wolframclient.utils.encoding import force_text
from json import loads as json_loads

import logging
logging.basicConfig(filename='/tmp/python_testsuites.log',
                    filemode='a',
                    format='%(asctime)s, %(name)s %(levelname)s %(message)s',
                    level=logging.WARNING)
logger = logging.getLogger(__name__)

class TestSession(unittest.TestCase):
    #TODO modify those before testing.
    user_config_file = '/private/etc/user_credentials.cfg'
    api_owner = 'dorianb'

    session = None
    
    @staticmethod
    def sak_credentials():
        return SecuredAuthenticationKey.from_config(
            sak_configuration().read(TestSession.user_config_file))

    @staticmethod
    def authenticated_session():
        '''Cached session'''
        if TestSession.session is None:
            session = WolframCloudSession.default()
            session.authenticate(TestSession.sak_credentials())
            TestSession.session = session
        return TestSession.session

    @classmethod
    def setUpClass(cls):
        cls.session = cls.authenticated_session()

    def test_sak_credentials(self):
        cred = self.sak_credentials()
        self.assertIsInstance(cred.consumer_key, string_types)
        self.assertIsInstance(cred.consumer_secret, string_types)

    def test_section_not_authorized(self):
        session = WolframCloudSession.default()
        self.assertEqual(session.authorized, False)
        self.assertEqual(session.is_xauth, None)
        
    def test_section_authorized_oauth(self):
        session = self.authenticated_session()
        self.assertEqual(session.authorized, True)
        self.assertEqual(session.is_xauth, False)

    def test_section_authorized_xauth(self):
        user_config = user_credential_configuration().read(self.user_config_file)
        user_cred = UserCredentials.from_config(user_config)
        session = WolframCloudSession.default().authenticate(user_cred)
        self.assertEqual(session.authorized, True)
        self.assertEqual(session.is_xauth, True)
        
    def test_section_api_call_no_param(self):
        url = 'api/private/requesterid'
        response = self.session.call(
            (self.api_owner, url), decoder=force_text)
        self.assertIn(self.api_owner, response.output)

    def test_section_api_call_one_param(self):
        url = 'api/private/stringreverse'
        response = self.session.call(
            (self.api_owner, url), 
            input_parameters={'str': 'abcde'}, decoder=force_text)
        self.assertEqual('"edcba"', response.output)

    def test_section_api_call_one_param_wrong(self):
        url = 'api/private/stringreverse'
        response = self.session.call((self.api_owner, url), decoder=force_text)
        self.assertFalse(response.success)
        field, _ = response.fields_in_error()[0]
        self.assertEqual(field, 'str')

    def test_public_api_call(self):
        url = "api/public/jsonrange"
        session = WolframCloudSession.default()
        self.assertFalse(session.authorized)
        response = session.call((self.api_owner, url),
            input_parameters={'i': 5},
            decoder=json_loads)
        self.assertEqual(response.output, list(range(1, 6)))
                                
    def test_section_api_call_two_param(self):
        api = (self.api_owner, 'api/private/range/formated/json')
        v_min, v_max, step = (1, 10, 2)
        response = self.session.call(api,
            input_parameters={
                'min': v_min,
                'max': v_max,
                'step':step
            },
            decoder=json_loads)
        if not response.success:
            logger.warning(response.failure)
        expected = list(range(v_min, v_max, step))
        self.assertListEqual(expected, response.output)

    def test_section_wl_error(self):
        api = (self.api_owner, "api/private/range/wlerror")
        i = 1
        response = self.session.call(api,
            input_parameters={
                'i' : i
            },
            decoder=json_loads)
        self.assertFalse(response.success)
        self.assertEqual(response.response.status_code, 500)
        
