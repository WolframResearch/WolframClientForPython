# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging

from wolframclient.evaluation.cloud.base import (OAuthAsyncSessionBase,
                                                 UserIDPassword)
from wolframclient.exception import AuthenticationException
from wolframclient.utils import six
from wolframclient.utils.api import aiohttp, oauth, ssl

logger = logging.getLogger(__name__)


class OAuthAIOHttpAsyncSessionBase(OAuthAsyncSessionBase):
    """ Asynchronous OAuth authentication class using aiohttp library for requests. """

    def __init__(self,
                 http_session,
                 server,
                 consumer_key,
                 consumer_secret,
                 signature_method=None,
                 client_class=None,
                 ssl_context_class=None):
        super().__init__(
            server,
            consumer_key,
            consumer_secret,
            signature_method=signature_method,
            client_class=client_class or oauth.Client)
        self.http_session = http_session
        self.ssl_context_class = ssl_context_class or ssl.SSLContext
        if self.server.certificate is not None:
            self._ssl_context = self.ssl_context_class()
            self._ssl_context.load_verify_locations(self.server.certificate)
            # self._ssl_context = ssl.create_default_context(cafile=self.server.certificate)
        else:
            self._ssl_context = None

    async def signed_request(self, uri, headers={}, data=None, method='POST'):
        """ Construct a signed request and send it."""
        if not self.authorized():
            await self.authenticate()

        req_headers = {}
        for k, v in headers.items():
            req_headers[k] = v
        sign_body = False

        # Payload Instances are not encoded (e.g: octet stream). Only FormData are.
        form_encoded = isinstance(data,
                                  aiohttp.FormData) and not data.is_multipart
        multipart = isinstance(data, aiohttp.FormData) and data.is_multipart
        # only form encoded body are signed.
        # Non multipart FormData are url encoded: need signed request. We need to get back the body
        # as a string.
        if form_encoded:
            buffer = _AsyncBytesIO()
            await data().write(buffer)
            body = buffer.getvalue()
            req_headers['Content-Type'] = 'application/x-www-form-urlencoded'

        uri, req_headers, signed_body = self._client.sign(
            uri,
            method,
            body=body if form_encoded else None,
            headers=req_headers,
            realm=self.server.cloudbase)
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Signed uri: %s', uri)
            logger.debug('Signed header: %s', req_headers)
            logger.debug('Is body signed: %s', form_encoded)

        if multipart or not form_encoded:
            body = data
        else:
            body = aiohttp.StringPayload(signed_body)
        return await self.http_session.request(
            method, uri, data=body, headers=req_headers, ssl=self._ssl_context)

    async def _ensure_success_response(self, response):
        msg = None
        if response.status == 200:
            return
        try:
            as_json = await response.json()
            msg = as_json.get('message', None)
        # msg is None if response is not JSON, but it's fine.
        except:
            raise AuthenticationException(
                response, 'Request failed with status %i' % response.status)
        raise AuthenticationException(response, msg)


class OAuth1AIOHttpAsyncSession(OAuthAIOHttpAsyncSessionBase):
    """ OAuth1 using aiohttp."""

    async def set_oauth_request_token(self):
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Fetching oauth request token from: %s',
                         self.server.request_token_endpoint)

        logging.disable(logging.DEBUG)

        token_client = self.client_class(
            self.consumer_key, client_secret=self.consumer_secret)
        uri, headers, body = token_client.sign(
            self.server.request_token_endpoint, "POST")

        logging.disable(logging.NOTSET)

        async with self.http_session.post(
                uri, headers=headers, data=body,
                ssl=self._ssl_context) as response:
            await self._ensure_success_response(response)
            self._update_token_from_request_body(await response.read())

    async def set_oauth_access_token(self):
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Fetching oauth access token from %s',
                         self.server.access_token_endpoint)
        access_client = self.client_class(
            self.consumer_key,
            client_secret=self.consumer_secret,
            resource_owner_key=self._oauth_token,
            resource_owner_secret=self._oauth_token_secret)
        uri, headers, body = access_client.sign(
            self.server.access_token_endpoint, "POST")
        async with self.http_session.post(
                uri, headers=headers, data=body,
                ssl=self._ssl_context) as response:
            await self._ensure_success_response(response)
            self._update_token_from_request_body(await response.read())

    async def authenticate(self):
        await self.set_oauth_request_token()
        await self.set_oauth_access_token()
        self._update_client()


class XAuthAIOHttpAsyncSession(OAuthAIOHttpAsyncSessionBase):
    """ XAuth using aiohttp."""

    def __init__(self,
                 userid_password,
                 http_session,
                 server,
                 signature_method=None,
                 client_class=oauth.Client):

        super().__init__(
            http_session,
            server,
            server.xauth_consumer_key,
            server.xauth_consumer_secret,
            signature_method=signature_method,
            client_class=client_class)
        if not self.server.is_xauth():
            raise AuthenticationException(
                'XAuth is not configured for this server. Missing xauth consumer key and/or secret.'
            )
        if isinstance(userid_password, tuple) and len(userid_password) == 2:
            self.xauth_credentials = UserIDPassword(*userid_password)
        elif isinstance(userid_password, UserIDPassword):
            self.xauth_credentials = userid_password
        else:
            raise ValueError(
                'User ID and password must be specified as a tuple or a UserIDPassword instance.'
            )

    async def authenticate(self):
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('xauth authentication of user %s',
                         self.xauth_credentials.user)
        client = self.client_class(self.consumer_key, self.consumer_secret)

        # avoid dumping password in log files.
        logging.disable(logging.DEBUG)

        uri, headers, body = client.sign(
            self.server.access_token_endpoint,
            'POST',
            headers=self.DEFAULT_CONTENT_TYPE,
            body={
                "x_auth_username": self.xauth_credentials.user,
                "x_auth_password": self.xauth_credentials.password,
                "x_auth_mode": "client_auth",
            })

        logging.disable(logging.NOTSET)

        async with self.http_session.post(
                uri, headers=headers, data=body,
                ssl=self._ssl_context) as response:
            await self._ensure_success_response(response)
            self._update_token_from_request_body(await response.read())

        self._update_client()


class _AsyncBytesIO(object):
    def __init__(self, initial_bytes=None):
        self.buffer = six.BytesIO(initial_bytes)

    async def write(self, value):
        self.buffer.write(value)

    def getvalue(self):
        self.buffer.flush()
        return self.buffer.getvalue()
