# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
import os
import unittest
import asyncio
from wolframclient.evaluation.cloud import WolframServer
from wolframclient.evaluation.cloud.asynccloudsession import (
    WolframCloudAsyncSession
    )

from wolframclient.evaluation.cloud.base import (SecuredAuthenticationKey,
                                                  UserIDPassword)
from wolframclient.exception import AuthenticationException, WolframLanguageException
from wolframclient.language import wl, wlexpr
from wolframclient.tests.configure import MSG_JSON_NOT_FOUND, json_config, user_configuration, secured_authentication_key, server
from wolframclient.utils import six
from wolframclient.utils.api import json
from wolframclient.utils.encoding import force_text
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.utils.asyncio import run_in_loop, get_event_loop

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

LOOP = get_event_loop()

@unittest.skipIf(json_config is None, MSG_JSON_NOT_FOUND)
class TestCaseSettings(BaseTestCase):
    user_cred = None
    server = None

    @classmethod
    def setUpClass(cls):
        cls.setupCloudSession()

    @classmethod
    def setupCloudSession(cls):
        cls.sak = secured_authentication_key
        cls.api_owner = json_config.get('ApiOwner', 'dorianb')
        cls.user_cred = user_configuration
        cls.server = server
        cls.cloud_session_async = WolframCloudAsyncSession(credentials=cls.sak)

    @classmethod
    def tearDownClass(cls):
        cls.tearDownCloudSession()

    @classmethod
    @run_in_loop
    async def tearDownCloudSession(cls):
        if cls.cloud_session_async is not None:
            await cls.cloud_session_async.stop()

    def get_data_path(self, filename):
        """Return full path of a file in ./data/directory"""
        current_file_dir = os.path.dirname(__file__)
        return os.path.join(current_file_dir, '..', 'data', filename)

@unittest.skipIf(json_config is None, MSG_JSON_NOT_FOUND)
@unittest.skipIf(six.JYTHON, "Not supported in Jython.")
class TestCase(TestCaseSettings):
    def test_section_not_authorized(self):
        session = WolframCloudAsyncSession()
        self.assertEqual(session.authorized(), False)
        self.assertEqual(session.anonymous(), True)
    
    @run_in_loop
    async def test_section_authorized_oauth(self):
        cloud_session = WolframCloudAsyncSession(credentials=self.sak)
        await cloud_session.start()
        self.assertEqual(cloud_session.authorized(), True)
        self.assertEqual(cloud_session.anonymous(), False)

    @run_in_loop
    async def test_section_authorized_oauth_with(self):
        async with WolframCloudAsyncSession(credentials=self.sak) as cloud_session:
            self.assertEqual(cloud_session.authorized(), True)
            self.assertEqual(cloud_session.anonymous(), False)

    @run_in_loop
    async def test_section_authorized_xauth(self):
        if self.user_cred and self.server:
            cloud_session = WolframCloudAsyncSession(
                credentials=self.user_cred, server=self.server)
            await cloud_session.start()
            self.assertEqual(cloud_session.authorized(), True)
            self.assertEqual(cloud_session.anonymous(), False)
        else:
            print('xauth not available. Test skipped.')
        
    @run_in_loop
    async def test_section_authorized_xauth_with(self):
        if self.user_cred and self.server:
            async with WolframCloudAsyncSession(
                credentials=self.user_cred, server=self.server) as cloud_session:
                self.assertEqual(cloud_session.authorized(), True)
                self.assertEqual(cloud_session.anonymous(), False)
        else:
            print('xauth not available. Test skipped.')

    @run_in_loop
    async def test_bad_sak(self):
        bad_sak = SecuredAuthenticationKey('foo', 'bar')
        with self.assertRaises(AuthenticationException):
            cloud_session = WolframCloudAsyncSession(credentials=bad_sak)
            await cloud_session.start()

    @run_in_loop
    async def test_bad_sak_with(self):
        bad_sak = SecuredAuthenticationKey('foo', 'bar')
        with self.assertRaises(AuthenticationException):
            async with WolframCloudAsyncSession(credentials=bad_sak) as cloud_session:
                cloud_session.authorized()

    @run_in_loop
    async def test_section_api_call_no_param(self):
        url = 'api/private/requesterid'
        response = await self.cloud_session_async.call((self.api_owner, url))
        self.assertIn(self.api_owner, force_text(await response.get()))

    @run_in_loop
    async def test_section_api_call_one_param(self):
        url = 'api/private/stringreverse'
        response = await self.cloud_session_async.call((self.api_owner, url),
                                           input_parameters={'str': 'abcde'})
        self.assertEqual('"edcba"', force_text(await response.get()))

    @run_in_loop
    async def test_section_api_call_one_param_wrong(self):
        url = 'api/private/stringreverse'
        response = await self.cloud_session_async.call((self.api_owner, url))
        self.assertFalse(response.success)
        field, _ = (await response.fields_in_error())[0]
        self.assertEqual(field, 'str')

    @run_in_loop
    async def test_public_api_call(self):
        url = "api/public/jsonrange"
        cloud_session = WolframCloudAsyncSession()
        self.assertFalse(cloud_session.authorized())
        self.assertTrue(cloud_session.anonymous())
        response = await cloud_session.call((self.api_owner, url),
                                      input_parameters={'i': 5})
        self.assertTrue(response.success)
        self.assertEqual(json.loads(await response.get()), list(range(1, 6)))

    @run_in_loop
    async def test_section_api_call_two_param(self):
        api = (self.api_owner, 'api/private/range/formated/json')
        v_min, v_max, step = (1, 10, 2)
        response = await self.cloud_session_async.call(
            api, input_parameters={
                'min': v_min,
                'max': v_max,
                'step': step
            })
        if not response.success:
            logger.warning(await response.failure)
        expected = list(range(v_min, v_max, step))
        self.assertListEqual(expected, json.loads(await response.get()))

    @run_in_loop
    async def test_section_invalid_api_path(self):
        with self.assertRaises(WolframLanguageException):
            api = (self.api_owner, 'invalid/api/path/no/resource')
            res = await self.cloud_session_async.call(api)
            await res.get()

    @run_in_loop
    async def test_section_wl_error(self):
        api = (self.api_owner, "api/private/range/wlerror")
        i = 1
        response = await self.cloud_session_async.call(api, input_parameters={'i': i})
        self.assertFalse(response.success)
        self.assertEqual(response.status, 500)

    @run_in_loop
    async def test_small_image_file(self):
        api = (self.api_owner, 'api/private/imagedimensions')
        with open(self.get_data_path('32x2.png'), 'rb') as fp:
            response = await self.cloud_session_async.call(api, files={'image': fp})
            self.assertTrue(response.success)
            res = json.loads(await response.get())
            self.assertListEqual(res, [32, 2])

    @run_in_loop
    async def test_image_file(self):
        api = (self.api_owner, 'api/private/imagedimensions')
        with open(self.get_data_path('500x200.png'), 'rb') as fp:
            response = await self.cloud_session_async.call(api, files={'image': fp})
            self.assertTrue(response.success)
            res = json.loads(await response.get())
            self.assertListEqual(res, [500, 200])

    @run_in_loop
    async def test_image_string_int(self):
        api = ('dorianb', 'api/private/str_image_int')
        with open(self.get_data_path('32x2.png'), 'rb') as fp:
            response = await self.cloud_session_async.call(api, input_parameters={'str':'abc', 'int' : 10}, files={'image': fp})
            self.assertTrue(response.success)
            res = json.loads(await response.get())
            self.assertListEqual(res, ['abc', [32, 2], 10])

    ### Evaluation
    
    @run_in_loop
    async def test_evaluate_string(self):
        res = await self.cloud_session_async.evaluate(wlexpr('Range[3]'))
        self.assertEqual(res, '{1, 2, 3}')

    @run_in_loop
    async def test_evaluate_wl_expr(self):
        res = await self.cloud_session_async.evaluate(wl.Range(2))
        self.assertEqual(res, '{1, 2}')

    @run_in_loop
    async def test_evaluate_wl_expr_option(self):
        res = await self.cloud_session_async.evaluate(wl.ArrayPad([[1]], 1, Padding=1))
        self.assertEqual(res, '{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}}')

    @run_in_loop
    async def test_evaluate_wrap(self):
        res = await self.cloud_session_async.evaluate_wrap(wl.Range(2))
        self.assertTrue(await res.success)
        self.assertEqual(await res.get(), '{1, 2}')

    @run_in_loop
    async def test_evaluate_function(self):
        f = self.cloud_session_async.function(wlexpr('Range'))
        self.assertEqual(await f(3), '{1, 2, 3}')

    @run_in_loop
    async def test_evaluate_function_wl(self):
        f = self.cloud_session_async.function(wl.Range)
        self.assertEqual(await f(3), '{1, 2, 3}')

    @run_in_loop
    async def test_evaluate_function_wl_option(self):
        f = self.cloud_session_async.function(wl.ArrayPad)
        self.assertEqual(
            await f([[1]], 1, Padding=1), '{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}}')

    @run_in_loop
    async def test_evaluate_string(self):
        res1 = await self.cloud_session_async.evaluate(wlexpr('Range[1]'))
        res2 = await self.cloud_session_async.evaluate(wlexpr('Range[2]'))

        self.assertEqual(res1, '{1}')
        self.assertEqual(res2, '{1, 2}')

    @run_in_loop
    async def test_evaluate_string_concurrently(self):
        task1 = asyncio.ensure_future(self.cloud_session_async.evaluate(wlexpr('Range[1]')))
        task2 = asyncio.ensure_future(self.cloud_session_async.evaluate_wrap(wlexpr('Range[2]')))
        res1, res2 = await asyncio.gather(task1, task2)
        self.assertEqual(res1, '{1}')
        res2= await res2.result
        self.assertEqual(res2, '{1, 2}')

#     # encode input parameters

#     def test_encode_wl(self):
#         encoded = encode_api_inputs({'param1': {'k': [1, 2]}, 'param2': 'foo'})
#         self.assertEqual(encoded, {
#             'param1': b'<|"k" -> {1, 2}|>',
#             'param2': 'foo'
#         })

#     def test_encode_empty_dict(self):
#         self.assertEqual(encode_api_inputs({}, target_format='json'), {})

#     def test_encode_json_dict(self):
#         encoded = encode_api_inputs({
#             'param1': {
#                 'k': [1, 2]
#             },
#             'param2': 'foo'
#         },
#                                     target_format='json')
#         self.assertEqual(encoded, {
#             'param1__json': '{"k": [1, 2]}',
#             'param2__json': '"foo"'
#         })

#     def test_encode_wxf_dict(self):
#         encoded = encode_api_inputs({
#             'param1': {
#                 'k': [1, 2]
#             },
#             'param2': 'foo'
#         },
#                                     target_format='wxf')
#         self.assertEqual(
#             encoded, {
#                 'param1__wxf': b'8:A\x01-S\x01kf\x02s\x04ListC\x01C\x02',
#                 'param2__wxf': b'8:S\x03foo'
#             })


# class TestWolframAPI(TestCaseSettings):
#     def test_wolfram_api_call_image(self):
#         api = (self.api_owner, 'api/private/imagedimensions')
#         apicall = WolframAPICall(self.cloud_session, api)
#         with open(self.get_data_path('32x2.png'), 'rb') as fp:
#             apicall.add_file_parameter('image', fp)
#             res = apicall.perform()
#             self.assertTrue(res.success)
#             res = json.loads(res.get())
#             self.assertListEqual(res, [32, 2])

#     def test_wolfram_api_call_str(self):
#         api = (self.api_owner, 'api/private/stringreverse')
#         apicall = WolframAPICall(self.cloud_session, api)
#         apicall.add_parameter('str', 'abcde')
#         res = apicall.perform().get()
#         self.assertEqual('"edcba"', force_text(res))

#     def test_wolfram_api_image_string_int(self):
#         api = ('dorianb', 'api/private/str_image_int')
#         with open(self.get_data_path('32x2.png'), 'rb') as fp:
#             apicall = WolframAPICall(self.cloud_session, api)
#             apicall.add_parameter('str', 'abc')
#             apicall.add_parameter('int', 10)
#             apicall.add_image_data_parameter('image', fp)
#             result = apicall.perform().get()
#             res = json.loads(result)
#             self.assertListEqual(res, ['abc', [32, 2], 10])