from __future__ import absolute_import, print_function, unicode_literals

import unittest
from wolframclient.utils.six import string_types, JYTHON

if not JYTHON:
    import requests
    from wolframclient.evaluation.cloud.cloudsession import WolframCloudSession, url_join
    from wolframclient.evaluation.cloud.oauth import SecuredAuthenticationKey, UserCredentials
    from wolframclient.utils.six import string_types
    from wolframclient.utils.encoding import force_text
    from json import loads as json_loads, load as json_load

import logging
logging.basicConfig(filename='/tmp/python_testsuites.log',
                    filemode='a',
                    format='%(asctime)s, %(name)s %(levelname)s %(message)s',
                    level=logging.WARNING)
logger = logging.getLogger(__name__)


@unittest.skipIf(JYTHON, "Not supported in Jython.")
class TestSession(unittest.TestCase):
    #TODO modify those before testing.
    user_config_file = '/private/etc/user_credentials.json'
    api_owner = 'dorianb'

    @classmethod
    def setUpClass(cls):
        with open(TestSession.user_config_file, 'r') as fp:
            cls.json_user_config = json_load(fp)
        cls.sak = SecuredAuthenticationKey(
            cls.json_user_config['SAK']['consumer_key'],
            cls.json_user_config['SAK']['consumer_secret']
            )
        cls.user_cred = UserCredentials(
            cls.json_user_config['User']['id'],
            cls.json_user_config['User']['password']
        )
        cls.session = WolframCloudSession(authentication=cls.sak)


@unittest.skipIf(JYTHON, "Not supported in Jython.")
class TestAPI(TestSession):
    def test_section_not_authorized(self):
        session = WolframCloudSession()
        self.assertEqual(session.authorized, False)
        self.assertEqual(session.is_xauth, None)
        
    def test_section_authorized_oauth(self):
        session = WolframCloudSession(authentication=self.sak)
        self.assertEqual(session.authorized, True)
        self.assertEqual(session.is_xauth, False)

    def test_section_authorized_xauth(self):
        session = WolframCloudSession(self.user_cred)
        self.assertEqual(session.authorized, True)
        self.assertEqual(session.is_xauth, True)
        
    def test_section_api_call_no_param(self):
        url = 'api/private/requesterid'
        response = self.session.call(
            (self.api_owner, url))
        self.assertIn(self.api_owner, force_text(response.output))

    def test_section_api_call_one_param(self):
        url = 'api/private/stringreverse'
        response = self.session.call(
            (self.api_owner, url), 
            input_parameters={'str': 'abcde'})
        self.assertEqual('"edcba"', force_text(response.output))

    def test_section_api_call_one_param_wrong(self):
        url = 'api/private/stringreverse'
        response = self.session.call((self.api_owner, url))
        self.assertFalse(response.success)
        field, _ = response.fields_in_error()[0]
        self.assertEqual(field, 'str')

    def test_public_api_call(self):
        url = "api/public/jsonrange"
        session = WolframCloudSession()
        self.assertFalse(session.authorized)
        response = session.call((self.api_owner, url),
            input_parameters={'i': 5})
        self.assertTrue(response.success)
        self.assertEqual(json_loads(response.output), list(range(1, 6)))
                                
    def test_section_api_call_two_param(self):
        api = (self.api_owner, 'api/private/range/formated/json')
        v_min, v_max, step = (1, 10, 2)
        response = self.session.call(api,
            input_parameters={
                'min': v_min,
                'max': v_max,
                'step':step
            })
        if not response.success:
            logger.warning(response.failure)
        expected = list(range(v_min, v_max, step))
        self.assertListEqual(expected, json_loads(response.output))

    def test_section_wl_error(self):
        api = (self.api_owner, "api/private/range/wlerror")
        i = 1
        response = self.session.call(api,
            input_parameters={
                'i' : i
            })
        self.assertFalse(response.success)
        self.assertEqual(response.response.status_code, 500)


@unittest.skipIf(JYTHON, "Not supported in Jython.")
class TestURLJoin(unittest.TestCase):
    def test_append_no_base(self):
        url = url_join('http://wolfram.com', 'foo')
        self.assertEqual(url, 'http://wolfram.com/foo')

    def test_simple_append(self):
        url = url_join('http://wolfram.com', 'foo')
        self.assertEqual(url, 'http://wolfram.com/foo')
        
    def test_simple_append_end_slash(self):
        url = url_join('http://wolfram.com/', 'foo')
        self.assertEqual(url, 'http://wolfram.com/foo')

    def test_simple_append_start_slash(self):
        url = url_join('http://wolfram.com', '/foo')
        self.assertEqual(url, 'http://wolfram.com/foo')

    def test_simple_append_two_slash(self):
        url = url_join('http://wolfram.com/', '/foo')
        self.assertEqual(url, 'http://wolfram.com/foo')

    def test_extend(self):
        url = url_join('http://wolfram.com/', 'foo','bar','baz')
        self.assertEqual(url, 'http://wolfram.com/foo/bar/baz')

    def test_extend_some_empty(self):
        url = url_join('http://wolfram.com/', 'foo', '', 'baz')
        self.assertEqual(url, 'http://wolfram.com/foo/baz')

    def test_extend_some_empty_slashes(self):
        url = url_join('http://wolfram.com/', 'foo/', '', '/baz')
        self.assertEqual(url, 'http://wolfram.com/foo/baz')
