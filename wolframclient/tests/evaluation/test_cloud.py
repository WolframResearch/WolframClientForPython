# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
import os
import unittest

from wolframclient.evaluation.cloud import WolframServer
from wolframclient.evaluation.cloud.cloudsession import (
    WolframAPICall, WolframCloudSession, WolframCloudSessionAsync,
    encode_api_inputs, url_join)
from wolframclient.evaluation.cloud.oauth import (SecuredAuthenticationKey,
                                                  UserIDPassword)
from wolframclient.language import wl
from wolframclient.tests.configure import MSG_JSON_NOT_FOUND, json_config
from wolframclient.utils import six
from wolframclient.utils.api import json
from wolframclient.utils.encoding import force_text
from wolframclient.utils.tests import TestCase as BaseTestCase

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


@unittest.skipIf(json_config is None, MSG_JSON_NOT_FOUND)
class TestCaseSettings(BaseTestCase):
    user_cred = None
    server = None

    @classmethod
    def setUpClass(cls):
        cls.setupCloudSession()

    @classmethod
    def setupCloudSession(cls):
        cloud_config = json_config['cloud_credentials']
        cls.sak = SecuredAuthenticationKey(
            cloud_config['SAK']['consumer_key'],
            cloud_config['SAK']['consumer_secret'])
        cls.api_owner = cloud_config.get('ApiOwner', 'dorianb')
        try:
            cls.user_cred = UserIDPassword(cloud_config['User']['id'],
                                           cloud_config['User']['password'])
            server_json = json_config['server']
            cls.server = WolframServer(
                server_json['host'],
                server_json['request_token_endpoint'],
                server_json['access_token_endpoint'],
                xauth_consumer_key=server_json['xauth_consumer_key'],
                xauth_consumer_secret=server_json['xauth_consumer_secret'])
        except KeyError as e:
            print(e)
            cls.user_cred = None
            cls.server = None

        cls.cloud_session = WolframCloudSession(authentication=cls.sak)
        cls.cloud_session_async = WolframCloudSessionAsync(
            authentication=cls.sak)

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


@unittest.skipIf(json_config is None, MSG_JSON_NOT_FOUND)
@unittest.skipIf(six.JYTHON, "Not supported in Jython.")
class TestCase(TestCaseSettings):
    def test_section_not_authorized(self):
        cloud_session = WolframCloudSession()
        self.assertEqual(cloud_session.authorized, False)
        self.assertEqual(cloud_session.is_xauth, None)

    def test_section_authorized_oauth(self):
        cloud_session = WolframCloudSession(authentication=self.sak)
        self.assertEqual(cloud_session.authorized, True)
        self.assertEqual(cloud_session.is_xauth, False)

    @unittest.skipUnless(TestCaseSettings.user_cred
                         and TestCaseSettings.server, "xauth not available.")
    def test_section_authorized_xauth(self):
        cloud_session = WolframCloudSession(
            authentication=self.user_cred, server=self.server)
        self.assertEqual(cloud_session.authorized, True)
        self.assertEqual(cloud_session.is_xauth, True)

    def test_section_api_call_no_param(self):
        url = 'api/private/requesterid'
        response = self.cloud_session.call((self.api_owner, url))
        self.assertIn(self.api_owner, force_text(response.get()))

    def test_section_api_call_one_param(self):
        url = 'api/private/stringreverse'
        response = self.cloud_session.call((self.api_owner, url),
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
        response = self.cloud_session.call(
            api, input_parameters={
                'min': v_min,
                'max': v_max,
                'step': step
            })
        if not response.success:
            logger.warning(response.failure)
        expected = list(range(v_min, v_max, step))
        self.assertListEqual(expected, json.loads(response.get()))

    def test_section_wl_error(self):
        api = (self.api_owner, "api/private/range/wlerror")
        i = 1
        response = self.cloud_session.call(api, input_parameters={'i': i})
        self.assertFalse(response.success)
        self.assertEqual(response.response.status_code, 500)

    def test_small_image_file(self):
        api = (self.api_owner, 'api/private/imagedimensions')
        with open(self.get_data_path('32x2.png'), 'rb') as fp:
            response = self.cloud_session.call(api, files={'image': fp})
            self.assertTrue(response.success)
            res = json.loads(response.get())
            self.assertListEqual(res, [32, 2])

    def test_image_file(self):
        api = (self.api_owner, 'api/private/imagedimensions')
        with open(self.get_data_path('500x200.png'), 'rb') as fp:
            response = self.cloud_session.call(api, files={'image': fp})
            self.assertTrue(response.success)
            res = json.loads(response.get())
            self.assertListEqual(res, [500, 200])

    ### Evaluation

    def test_evaluate_string(self):
        res = self.cloud_session.evaluate('Range[3]')
        self.assertEqual(res, '{1, 2, 3}')

    def test_evaluate_wl_expr(self):
        res = self.cloud_session.evaluate(wl.Range(2))
        self.assertEqual(res, '{1, 2}')

    def test_evaluate_wl_expr_option(self):
        res = self.cloud_session.evaluate(wl.ArrayPad([[1]], 1, Padding=1))
        self.assertEqual(res, '{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}}')

    def test_evaluate_wrap(self):
        res = self.cloud_session.evaluate_wrap(wl.Range(2))
        self.assertTrue(res.success)
        self.assertEqual(res.get(), '{1, 2}')

    def test_evaluate_function(self):
        f = self.cloud_session.function('Range')
        self.assertEqual(f(3), '{1, 2, 3}')

    def test_evaluate_function_wl(self):
        f = self.cloud_session.function(wl.Range)
        self.assertEqual(f(3), '{1, 2, 3}')

    def test_evaluate_function_wl_option(self):
        f = self.cloud_session.function(wl.ArrayPad)
        self.assertEqual(
            f([[1]], 1, Padding=1), '{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}}')

    def test_evaluate_string(self):
        res1 = self.cloud_session_async.evaluate('Range[1]')
        res2 = self.cloud_session_async.evaluate('Range[2]')

        self.assertEqual(res1.result(), '{1}')
        self.assertEqual(res2.result(), '{1, 2}')

    def test_evaluate_string(self):
        res = self.cloud_session_async.evaluate('Range[3]')
        self.assertEqual(res, '{1, 2, 3}')

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
        url = url_join('http://wolfram.com/', 'foo', 'bar', 'baz')
        self.assertEqual(url, 'http://wolfram.com/foo/bar/baz')

    def test_extend_some_empty(self):
        url = url_join('http://wolfram.com/', 'foo', '', 'baz')
        self.assertEqual(url, 'http://wolfram.com/foo/baz')

    def test_extend_some_empty_slashes(self):
        url = url_join('http://wolfram.com/', 'foo/', '', '/baz')
        self.assertEqual(url, 'http://wolfram.com/foo/baz')

    # encode input parameters

    def test_encode_wl(self):
        encoded = encode_api_inputs({'param1': {'k': [1, 2]}, 'param2': 'foo'})
        self.assertEqual(encoded, {
            'param1': b'<|"k" -> {1, 2}|>',
            'param2': 'foo'
        })

    def test_encode_empty_dict(self):
        self.assertEqual(encode_api_inputs({}, target_format='json'), {})

    def test_encode_json_dict(self):
        encoded = encode_api_inputs({
            'param1': {
                'k': [1, 2]
            },
            'param2': 'foo'
        },
                                    target_format='json')
        self.assertEqual(encoded, {
            'param1__json': '{"k": [1, 2]}',
            'param2__json': '"foo"'
        })

    def test_encode_wxf_dict(self):
        encoded = encode_api_inputs({
            'param1': {
                'k': [1, 2]
            },
            'param2': 'foo'
        },
                                    target_format='wxf')
        self.assertEqual(
            encoded, {
                'param1__wxf': b'8:A\x01-S\x01kf\x02s\x04ListC\x01C\x02',
                'param2__wxf': b'8:S\x03foo'
            })


class TestWolframAPI(TestCaseSettings):
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
