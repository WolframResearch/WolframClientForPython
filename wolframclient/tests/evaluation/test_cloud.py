# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
import unittest
import os

from wolframclient.logger.utils import setup_logging_to_file
from wolframclient.utils import six
from wolframclient.utils.api import json
from wolframclient.utils.encoding import force_text
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.language import wl
from wolframclient.serializers import export
if not six.JYTHON:
    from wolframclient.evaluation import SecuredAuthenticationKey, UserIDPassword, WolframCloudSession, WolframCloudSessionAsync
    from wolframclient.evaluation.cloud.cloudsession import encode_api_inputs, url_join

setup_logging_to_file('/tmp/python_testsuites.log', level=logging.WARNING)
logger = logging.getLogger(__name__)


class TestCaseSettings(BaseTestCase):
    #TODO modify those before testing.
    ''' Example json file for `user_credentials.json`:
    ```json
    {
        "User" : {
            "id" : "email@wolfram.com",
            "password" : "password"
        },
        "SAK" : {
            "consumer_key" : "xxxx",
            "consumer_secret" : "yyyy"
        },
        "ApiOwner" : "userID"
    }
    ```
    '''
    user_config_file = '/private/etc/user_credentials.json'

    @classmethod
    def setUpClass(cls):
        cls.setupCloudSession()

    @classmethod
    def setupCloudSession(cls):
        with open(TestCase.user_config_file, 'r') as fp:
            cls.json_user_config = json.load(fp)
        cls.sak = SecuredAuthenticationKey(
            cls.json_user_config['SAK']['consumer_key'],
            cls.json_user_config['SAK']['consumer_secret']
            )
        cls.user_cred = UserIDPassword(
            cls.json_user_config['User']['id'],
            cls.json_user_config['User']['password']
        )
        cls.api_owner = cls.json_user_config.get('ApiOwner', 'dorianb')

        cls.cloud_session = WolframCloudSession(authentication=cls.sak)
        cls.cloud_session_async = WolframCloudSessionAsync(authentication=cls.sak)

    @classmethod
    def tearDownClass(cls):
        cls.tearDownCloudSession()

    @classmethod
    def tearDownCloudSession(cls):
        if cls.cloud_session_async is not None:
            cls.cloud_session_async.terminate()

    def get_data_path(self, filename):
        """Return full path of a file in ./data/directory"""
        current_file_dir = os.path.dirname(__file__)
        return os.path.join(current_file_dir, '..', 'data', filename)

@unittest.skipIf(six.JYTHON, "Not supported in Jython.")
@unittest.skipIf(not os.path.exists(TestCaseSettings.user_config_file), "Need to configure credentials")
class TestCase(TestCaseSettings):

    def test_section_not_authorized(self):
        cloud_session = WolframCloudSession()
        self.assertEqual(cloud_session.authorized, False)
        self.assertEqual(cloud_session.is_xauth, None)

    def test_section_authorized_oauth(self):
        cloud_session = WolframCloudSession(authentication=self.sak)
        self.assertEqual(cloud_session.authorized, True)
        self.assertEqual(cloud_session.is_xauth, False)

    def test_section_authorized_xauth(self):
        cloud_session = WolframCloudSession(self.user_cred)
        self.assertEqual(cloud_session.authorized, True)
        self.assertEqual(cloud_session.is_xauth, True)

    def test_section_api_call_no_param(self):
        url = 'api/private/requesterid'
        response = self.cloud_session.call((self.api_owner, url))
        self.assertIn(self.api_owner, force_text(response.get()))

    def test_section_api_call_one_param(self):
        url = 'api/private/stringreverse'
        response = self.cloud_session.call(
            (self.api_owner, url),
            input_parameters={'str': 'abcde'})
        self.assertEqual('"edcba"', force_text(response.get()))

    def test_section_api_call_one_param_wrong(self):
        url = 'api/private/stringreverse'
        response = self.cloud_session.call((self.api_owner, url))
        self.assertFalse(response.success)
        field, _ = response.fields_in_error()[0]
        self.assertEqual(field, 'str')

    def test_public_api_call(self):
        url = "api/public/jsonrange"
        cloud_session = WolframCloudSession()
        self.assertFalse(cloud_session.authorized)
        response = cloud_session.call((self.api_owner, url),
            input_parameters={'i': 5})
        self.assertTrue(response.success)
        self.assertEqual(json.loads(response.get()), list(range(1, 6)))

    def test_section_api_call_two_param(self):
        api = (self.api_owner, 'api/private/range/formated/json')
        v_min, v_max, step = (1, 10, 2)
        response = self.cloud_session.call(api,
            input_parameters={
                'min': v_min,
                'max': v_max,
                'step':step
            })
        if not response.success:
            logger.warning(response.failure)
        expected = list(range(v_min, v_max, step))
        self.assertListEqual(expected, json.loads(response.get()))

    def test_section_wl_error(self):
        api = (self.api_owner, "api/private/range/wlerror")
        i = 1
        response = self.cloud_session.call(api,
            input_parameters={
                'i' : i
            })
        self.assertFalse(response.success)
        self.assertEqual(response.response.status_code, 500)

    def test_small_image_file(self):
        api = (self.api_owner, 'api/private/imagedimensions')
        with open(self.get_data_path('32x2.png'), 'rb') as fp:
            response = self.cloud_session.call(api, files={'image': fp})
            self.assertTrue(response.success)
            self.assertEqual(response.get(), b'{32, 2}')
    
    def test_image_file(self):
        api = (self.api_owner, 'api/private/imagedimensions')
        with open(self.get_data_path('500x200.png'), 'rb') as fp:
            response = self.cloud_session.call(api, files={'image': fp})
            self.assertTrue(response.success)
            self.assertEqual(response.get(), b'{500, 200}')


    # url_join

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

    # encode input parameters

    def test_encode_wl(self):
        encoded = encode_api_inputs(
            {'param1': {'k': [1, 2]}, 'param2': 'foo'})
        self.assertEqual(
            encoded, {'param1': b'<|"k" :> {1, 2}|>', 'param2': 'foo'})

    def test_encode_empty_dict(self):
        self.assertEqual(encode_api_inputs({}, target_format='json'), {})

    def test_encode_json_dict(self):
        encoded = encode_api_inputs({'param1' : {'k' : [1,2]}, 'param2' : 'foo'}, target_format='json')
        self.assertEqual(
            encoded, {'param1__json': '{"k": [1, 2]}', 'param2__json': '"foo"'})

    def test_encode_wxf_dict(self):
        encoded = encode_api_inputs(
            {'param1': {'k': [1, 2]}, 'param2': 'foo'}, target_format='wxf')
        self.assertEqual(
            encoded, {'param1__wxf': b'8:A\x01:S\x01kf\x02s\x04ListC\x01C\x02', 'param2__wxf': b'8:S\x03foo'})
