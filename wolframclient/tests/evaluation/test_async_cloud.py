# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import asyncio
import logging
import os
import unittest

from wolframclient.evaluation.cloud.asynccloudsession import (
    WolframAPICallAsync, WolframCloudAsyncSession)
from wolframclient.evaluation.cloud.base import (SecuredAuthenticationKey,
                                                 UserIDPassword)
from wolframclient.exception import (AuthenticationException, RequestException,
                                     WolframLanguageException)
from wolframclient.language import wl
from wolframclient.language.expression import WLFunction
from wolframclient.tests.configure import (MSG_JSON_NOT_FOUND, json_config,
                                           secured_authentication_key, server,
                                           user_configuration)
from wolframclient.utils import six
from wolframclient.utils.asyncio import get_event_loop, run_in_loop
from wolframclient.utils.encoding import force_text
from wolframclient.utils.tests import TestCase as BaseTestCase

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
        cls.api_owner = json_config['ApiOwner']
        cls.user_cred = user_configuration
        cls.server = server
        cls.cloud_session_async = WolframCloudAsyncSession(
            credentials=cls.sak, server=server)

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
        session = WolframCloudAsyncSession(server=self.server)
        self.assertEqual(session.authorized(), False)
        self.assertEqual(session.anonymous(), True)

    @run_in_loop
    async def test_section_authorized_oauth(self):
        cloud_session = WolframCloudAsyncSession(
            credentials=self.sak, server=self.server)
        try:
            await cloud_session.start()
            self.assertEqual(cloud_session.authorized(), True)
            self.assertEqual(cloud_session.anonymous(), False)
        finally:
            await cloud_session.terminate()

    @run_in_loop
    async def test_section_authorized_oauth_with(self):
        async with WolframCloudAsyncSession(
                credentials=self.sak, server=self.server) as cloud_session:
            self.assertEqual(cloud_session.authorized(), True)
            self.assertEqual(cloud_session.anonymous(), False)

    @run_in_loop
    async def test_section_authorized_xauth(self):
        if self.user_cred and self.server:
            cloud_session = WolframCloudAsyncSession(
                credentials=self.user_cred, server=self.server)
            try:
                await cloud_session.start()
                self.assertEqual(cloud_session.authorized(), True)
                self.assertEqual(cloud_session.anonymous(), False)
            finally:
                await cloud_session.terminate()
        else:
            print('xauth not available. Test skipped.')

    @run_in_loop
    async def test_section_authorized_xauth_with(self):
        if self.user_cred and self.server:
            async with WolframCloudAsyncSession(
                    credentials=self.user_cred,
                    server=self.server) as cloud_session:
                self.assertEqual(cloud_session.authorized(), True)
                self.assertEqual(cloud_session.anonymous(), False)
        else:
            print('xauth not available. Test skipped.')

    @run_in_loop
    async def test_bad_sak(self):
        bad_sak = SecuredAuthenticationKey('foo', 'bar')
        with self.assertRaises(AuthenticationException):
            cloud_session = WolframCloudAsyncSession(credentials=bad_sak, server=server)
            await cloud_session.start()

    @run_in_loop
    async def test_need_auth_err(self):
        bad_sak = SecuredAuthenticationKey('foo', 'bar')
        with self.assertRaises(RequestException):
            async with WolframCloudAsyncSession(server=server) as cloud_session:
                await cloud_session.evaluate('1+1')

    @run_in_loop
    async def test_bad_sak_with(self):
        bad_sak = SecuredAuthenticationKey('foo', 'bar')
        with self.assertRaises(RequestException):
            async with WolframCloudAsyncSession(
                    credentials=bad_sak, server=server) as cloud_session:
                cloud_session.authorized()

    @run_in_loop
    async def test_section_api_call_no_param(self):
        url = 'api/private/requesterid'
        response = await self.cloud_session_async.call((self.api_owner, url))
        self.assertIn(self.api_owner, force_text(await response.get()))

    @run_in_loop
    async def test_section_api_call_one_param(self):
        url = 'api/private/stringreverse'
        response = await self.cloud_session_async.call(
            (self.api_owner, url), input_parameters={'str': 'abcde'})
        self.assertEqual('"edcba"', force_text(await response.get()))

    @run_in_loop
    async def test_section_api_permission_key(self):
        async with WolframCloudAsyncSession(server=server) as cloud:
            url = 'api/public/permkey_stringreverse_wxf'
            response = await cloud.call((self.api_owner, url),
                                        input_parameters={'str': 'abcde'},
                                        permissions_key='my_key')
            self.assertEqual('edcba', await response.get())

    # currently missing key result in a webpage with an input field for the key.
    # @run_in_loop
    # async def test_section_api_missing_permission_key(self):
    #     url = 'api/public/permkey_stringreverse_wxf'
    #     with self.assertRaises(AuthenticationException):
    #         await self.cloud_session_async.call((self.api_owner, url), input_parameters={'str': 'abcde'})

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
        cloud_session = WolframCloudAsyncSession(server=self.server)
        try:
            self.assertFalse(cloud_session.authorized())
            self.assertTrue(cloud_session.anonymous())
            response = await cloud_session.call((self.api_owner, url),
                                                input_parameters={'i': 5})
            self.assertTrue(response.success)
            self.assertEqual(await response.get(), list(range(1, 6)))
        finally:
            await cloud_session.terminate()

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
        self.assertListEqual(expected, await response.get())

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
        response = await self.cloud_session_async.call(
            api, input_parameters={'i': i})
        self.assertFalse(response.success)
        self.assertEqual(response.status, 500)

    @run_in_loop
    async def test_small_image_file(self):
        api = (self.api_owner, 'api/private/imagedimensions')
        with open(self.get_data_path('32x2.png'), 'rb') as fp:
            response = await self.cloud_session_async.call(
                api, files={'image': fp})
            self.assertTrue(response.success)
            res = await response.get()
            self.assertListEqual(res, [32, 2])

    @run_in_loop
    async def test_image_file(self):
        api = (self.api_owner, 'api/private/imagedimensions')
        with open(self.get_data_path('500x200.png'), 'rb') as fp:
            response = await self.cloud_session_async.call(
                api, files={'image': fp})
            self.assertTrue(response.success)
            res = await response.get()
            self.assertListEqual(res, [500, 200])

    @run_in_loop
    async def test_image_string_int(self):
        api = (self.api_owner, 'api/private/str_image_int')
        with open(self.get_data_path('32x2.png'), 'rb') as fp:
            response = await self.cloud_session_async.call(
                api,
                input_parameters={
                    'str': 'abc',
                    'int': 10
                },
                files={'image': fp})
            self.assertTrue(response.success)
            res = await response.get()
            self.assertListEqual(res, ['abc', [32, 2], 10])

    @run_in_loop
    async def test_xml_valid_response(self):
        api = ('dorianb', 'api/private/rangeXML')
        response = await self.cloud_session_async.call(
            api, input_parameters={'i': 5})
        self.assertTrue(response.success)
        self.assertEqual(response.status, 200)

    @run_in_loop
    async def test_xml_invalid_response(self):
        api = ('dorianb', 'api/private/rangeXML')
        response = await self.cloud_session_async.call(api)
        self.assertFalse(response.success)
        self.assertEqual(response.status, 400)
        with self.assertRaises(WolframLanguageException):
            await response.get()

    @run_in_loop
    async def test_evaluate_string_disable(self):
        async with WolframCloudAsyncSession(
                credentials=self.sak,
                server=self.server,
                inputform_string_evaluation=False) as session:
            res = await session.evaluate('Range[3]')
            self.assertEqual(res, 'Range[3]')
            cor = session.function('f')
            res = await cor('abc')
            self.assertEqual(res, WLFunction('f', 'abc'))

    @run_in_loop
    async def test_stop_start_restart_status(self):
        session = WolframCloudAsyncSession(
            credentials=self.sak, server=self.server)
        try:
            self.assertFalse(session.started)
            self.assertTrue(session.stopped)
            await session.start()
            self.assertTrue(session.started)
            self.assertFalse(session.stopped)
            await session.stop()
            self.assertFalse(session.started)
            self.assertTrue(session.stopped)
            await session.restart()
            self.assertTrue(session.started)
            self.assertFalse(session.stopped)
            await session.terminate()
            self.assertFalse(session.started)
            self.assertTrue(session.stopped)
        finally:
            await session.terminate()

    ### Evaluation

    @run_in_loop
    async def test_evaluate_string(self):
        res = await self.cloud_session_async.evaluate('Range[3]')
        self.assertEqual(res, [1, 2, 3])

    @run_in_loop
    async def test_evaluate_wl_expr(self):
        res = await self.cloud_session_async.evaluate(wl.Range(2))
        self.assertEqual(res, [1, 2])

    @run_in_loop
    async def test_evaluate_wl_expr_option(self):
        res = await self.cloud_session_async.evaluate(
            wl.ArrayPad([[1]], 1, Padding=1))
        self.assertEqual(res, [[1, 1, 1], [1, 1, 1], [1, 1, 1]])

    @run_in_loop
    async def test_evaluate_wrap(self):
        res = await self.cloud_session_async.evaluate_wrap(wl.Range(2))
        self.assertTrue(await res.success)
        self.assertEqual(await res.get(), [1, 2])

    @run_in_loop
    async def test_evaluate_function(self):
        f = self.cloud_session_async.function('Range')
        self.assertEqual(await f(3), [1, 2, 3])

    @run_in_loop
    async def test_evaluate_function_wl(self):
        f = self.cloud_session_async.function(wl.Range)
        self.assertEqual(await f(3), [1, 2, 3])

    @run_in_loop
    async def test_evaluate_function_wl_option(self):
        f = self.cloud_session_async.function(wl.ArrayPad)
        self.assertEqual(await f([[1]], 1, Padding=1),
                         [[1, 1, 1], [1, 1, 1], [1, 1, 1]])

    @run_in_loop
    async def test_evaluate_string(self):
        res1 = await self.cloud_session_async.evaluate('Range[1]')
        res2 = await self.cloud_session_async.evaluate('Range[2]')

        self.assertEqual(res1, [1])
        self.assertEqual(res2, [1, 2])

    @run_in_loop
    async def test_evaluate_string_concurrently(self):
        task1 = asyncio.ensure_future(
            self.cloud_session_async.evaluate('Range[1]'))
        task2 = asyncio.ensure_future(
            self.cloud_session_async.evaluate_wrap('Range[2]'))
        res1, res2 = await asyncio.gather(task1, task2)
        self.assertEqual(res1, [1])
        res2 = await res2.result
        self.assertEqual(res2, [1, 2])

    # @run_in_loop
    # async def test_big_expr(self):
    #     a=numpy.ndarray((1000,1000), dtype='uint64')
    #     a.fill(1)
    #     total = await self.cloud_session_async.evaluate(wl.Total(a))
    #     self.assertEqual(total, 1000 * 1000)


class TestWolframAPI(TestCaseSettings):
    @run_in_loop
    async def test_wolfram_api_call_image(self):
        api = (self.api_owner, 'api/private/imagedimensions')
        apicall = WolframAPICallAsync(self.cloud_session_async, api)
        with open(self.get_data_path('32x2.png'), 'rb') as fp:
            apicall.add_file_parameter('image', fp)
            res = await apicall.perform()
            self.assertTrue(res.success)
            res = await res.get()
            self.assertListEqual(res, [32, 2])

    @run_in_loop
    async def test_wolfram_api_call_named_image(self):
        api = (self.api_owner, 'api/private/imagedimensions')
        apicall = WolframAPICallAsync(self.cloud_session_async, api)
        with open(self.get_data_path('32x2.png'), 'rb') as fp:
            apicall.add_file_parameter('image', fp, filename='testimage')
            res = await apicall.perform()
            self.assertTrue(res.success)
            res = await res.get()
            self.assertListEqual(res, [32, 2])

    @run_in_loop
    async def test_wolfram_api_from_session(self):
        api = (self.api_owner, 'api/private/imagedimensions')
        apicall = self.cloud_session_async.wolfram_api_call(api)
        with open(self.get_data_path('32x2.png'), 'rb') as fp:
            apicall.add_file_parameter('image', fp)
            res = await apicall.perform()
            self.assertTrue(res.success)
            res = await res.get()
            self.assertListEqual(res, [32, 2])

    @run_in_loop
    async def test_wolfram_api_call_str(self):
        api = (self.api_owner, 'api/private/stringreverse')
        apicall = WolframAPICallAsync(self.cloud_session_async, api)
        apicall.set_parameter('str', 'abcde')
        res = await apicall.perform()
        self.assertEqual('"edcba"', force_text(await res.get()))

    @run_in_loop
    async def test_wolfram_api_image_string_int(self):
        api = (self.api_owner, 'api/private/str_image_int')
        with open(self.get_data_path('32x2.png'), 'rb') as fp:
            apicall = WolframAPICallAsync(self.cloud_session_async, api)
            apicall.set_parameter('str', 'abc')
            apicall.set_parameter('int', 10)
            apicall.add_file_parameter('image', fp)
            result = await apicall.perform()
            res = await result.get()
            self.assertListEqual(res, ['abc', [32, 2], 10])

    @run_in_loop
    async def test_wolfram_api_imagebytes_string_int(self):
        api = (self.api_owner, 'api/private/str_image_int')
        with open(self.get_data_path('32x2.png'), 'rb') as fp:
            buffer = fp.read()
        apicall = WolframAPICallAsync(self.cloud_session_async, api)
        apicall.set_parameter('str', 'abc')
        apicall.set_parameter('int', 10)
        apicall.add_image_data_parameter('image', buffer)
        result = await apicall.perform()
        res = await result.get()
        self.assertListEqual(res, ['abc', [32, 2], 10])

    @run_in_loop
    async def test_api_invalid_input(self):
        api_urls = ('api/private/two_parameters_out_json',
                    'api/private/two_parameters_out_wxf',
                    'api/private/two_parameters_out_default')
        for url in api_urls:
            api = (self.api_owner, url)
            apicall = WolframAPICallAsync(self.cloud_session_async, api)
            apicall.set_parameter('x', 'abc')
            res = await apicall.perform()
            self.assertFalse(res.success)

    @run_in_loop
    async def test_api_permission_key(self):
        async with WolframCloudAsyncSession(server=server) as cloud:
            url = 'api/public/permkey_stringreverse_wxf'
            api = (self.api_owner, url)
            apicall = WolframAPICallAsync(cloud, api, permission_key='my_key')
            apicall.set_parameter('str', 'abcde')
            response = await apicall.perform()
            self.assertEqual('edcba', await response.get())
