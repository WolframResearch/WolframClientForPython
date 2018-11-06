# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
import logging
import json
from wolframclient.utils.api import oauth, urllib
from wolframclient.utils.url import url_join
from wolframclient.evaluation.cloud.server import WolframPublicCloudServer


__all__ = ['SecuredAuthenticationKey', 'UserIDPassword', 
    'OAuthSessionBase', 'OAuthAsyncSessionBase', 'OAuthSyncSessionBase']


class SecuredAuthenticationKey(object):
    ''' Represents a Secured Authentication Key generated using the Wolfram Language
    function `GenerateSecuredAuthenticationKey[]`

    It is used as an input when authenticating a cloud session.
    '''
    __slots__ = 'consumer_key', 'consumer_secret'
    is_xauth = False

    def __init__(self, consumer_key, consumer_secret):
        self.consumer_key = consumer_key
        self.consumer_secret = consumer_secret


class UserIDPassword(object):
    ''' Represents user credentials used to login to a cloud.

    It is used as an input when authenticating a cloud session.
    '''
    __slots__ = 'user', 'password'
    is_xauth = True

    def __init__(self, user, password):
        self.user = user
        self.password = password


class OAuthSessionBase(object):
    DEFAULT_CONTENT_TYPE = {
        'Content-Type': 'application/x-www-form-urlencoded',
        'User-Agent': 'WolframClientForPython/1.0'
    }

    def __init__(self,
                server,
                consumer_key,
                consumer_secret,
                signature_method=None,
                client_class=oauth.Client):
        self.consumer_key = consumer_key
        self.consumer_secret = consumer_secret
        self.signature_method = signature_method or oauth.SIGNATURE_HMAC
        self.client_class = client_class
        self._client = None
        self._oauth_token = None
        self._oauth_token_secret = None
        self.server = server

    def authorized(self):
        """Return a reasonnably accurate state of the authentication status."""
        return self._client is not None and bool(
            self._client.client_secret) and bool(
                self._client.resource_owner_key) and bool(
                    self._client.resource_owner_secret)

    def _update_client(self):
        self._client = self.client_class(
            self.consumer_key,
            client_secret=self.consumer_secret,
            resource_owner_key=self._oauth_token,
            resource_owner_secret=self._oauth_token_secret,
            signature_type=oauth.SIGNATURE_TYPE_AUTH_HEADER,
            realm=self.server.cloudbase,
            encoding='iso-8859-1')
    
    def _update_token_from_request_body(self, body):
        try:
            token = json.loads(body)
            self._oauth_token = token['oauth_token']
            self._oauth_token_secret = token['oauth_token_secret']
        except:
            token = urllib.parse_qs(body)
            self._oauth_token = token[b'oauth_token'][0]
            self._oauth_token_secret = token[b'oauth_token_secret'][0]

class OAuthSyncSessionBase(OAuthSessionBase):
    def authenticate(self):
        raise NotImplementedError

    def signed_request(self, uri, headers={}, data=None, method='POST'):
        raise NotImplementedError

class OAuthAsyncSessionBase(OAuthSessionBase):
    async def authenticate(self):
        raise NotImplementedError

    async def signed_request(self, uri, headers={}, data=None, method='POST'):
        raise NotImplementedError
