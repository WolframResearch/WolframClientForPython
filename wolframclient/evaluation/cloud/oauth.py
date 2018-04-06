# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
import os
import logging
try: # PY2
    from urlparse import urlparse, parse_qs, urlencode
except ImportError: # PY3
    from urllib.parse import urlparse, parse_qs, urlencode
import requests
from oauthlib import oauth1 as oauth

from wolframclient.evaluation.cloud.server import WolframPublicCloudServer
from wolframclient.evaluation.cloud.exceptions import RequestException, AuthenticationException, XAuthNotConfigured, ConfigurationException

logger = logging.getLogger(__name__)

# WOLFRAM_CERT_URL = "http://wolframca.wolfram.com/WolframCA3.crt"
# WOLFRAM_CERT_FILE = os.path.abspath(os.path.join(
#     os.path.dirname(__file__), '..', '..', "WolframCA3.crt"))
# print('certificat: ', WOLFRAM_CERT_FILE)
# import warnings
# from urllib3.exceptions import InsecureRequestWarning
# warnings.filterwarnings("ignore", category=InsecureRequestWarning)



class SecuredAuthenticationKey(object):
    __slots__ = 'consumer_key', 'consumer_secret'
    is_xauth = False
    def __init__(self, consumer_key, consumer_secret):
        self.consumer_key = consumer_key
        self.consumer_secret = consumer_secret

    @staticmethod
    def from_config(config):
        if hasattr(config, 'sak_consumer_key') and hasattr(config, 'sak_consumer_secret'):
            return SecuredAuthenticationKey(config.sak_consumer_key, config.sak_consumer_secret)
        else:
            raise ConfigurationException(
                'Consumer key and secret missing from configuration.')

class UserCredentials(object):
    __slots__ = 'user', 'password'
    is_xauth = True
    def __init__(self, user, password):
        self.user = user
        self.password = password

    @staticmethod
    def from_config(config):
        if hasattr(config, 'user_user_id') and hasattr(config, 'user_password'):
            return UserCredentials(config.user_user_id, config.user_password)
        else:
            raise ConfigurationException('User credentials missing from configuration.')

class OAuthSession(object):
    __slots__ = 'consumer_key', 'consumer_secret', 'signature_method',  '_oauth_token', '_oauth_token_secret', '_base_header', '_client', 'server_context', '_session'
    DEFAULT_CONTENT_TYPE = {
        'Content-Type': 'application/x-www-form-urlencoded',
        'User-Agent': 'WolframClientForPython/1.0'}

    def __init__(self, consumer_key, consumer_secret, signature_method="HMAC-SHA1", server_context=WolframPublicCloudServer):
        self.consumer_key = consumer_key
        self.consumer_secret = consumer_secret
        self.signature_method = signature_method
        self._base_header = OAuthSession.DEFAULT_CONTENT_TYPE
        self._client = None
        self._session = None
        self._oauth_token = None
        self._oauth_token_secret = None
        self.server_context = server_context
    
    def _check_response(self, response):
        # TODO: deal with error code with more precision.
        logger.debug('[Auth] Code: %i\nBody: %s', response.status_code, response.text)
        if response.status_code != 200:
            raise AuthenticationException(response)

    def _update_client(self):
        self._client = oauth.Client(self.consumer_key,
                                    client_secret=self.consumer_secret,
                                    resource_owner_key=self._oauth_token,
                                    resource_owner_secret=self._oauth_token_secret)

    # TODO add headers? Add a session and prepared requests?
    def signed_request(self, uri, body={}, method='POST'):
        if self._client is None:
            raise AuthenticationException('Not authenticated.')
        uri, headers, body = self._client.sign(
            uri,
            method,
            body=urlencode(body),
            headers=self._base_header
        )
        return requests.request(method, uri, headers=headers, data=body)        

    @property
    def authorized(self):
        return self._client is not None and bool(self._client.client_secret) and bool(self._client.resource_owner_key) and bool(self._client.resource_owner_secret)

    def _parse_oauth_response(self, response):
        token = parse_qs(response.text)
        return (token.get('oauth_token')[0],
            token.get('oauth_token_secret')[0])

    def xauth(self, user, password):
        logger.debug('xauth authentication of user %s', user)
        if not self.server_context.is_xauth():
            raise XAuthNotConfigured
        #todo use xauth server key/secret
        client = oauth.Client(self.consumer_key, self.consumer_secret)
        params = {}
        params["x_auth_username"] = user
        params["x_auth_password"] = password
        params["x_auth_mode"] = "client_auth"

        uri, headers, body = client.sign(
            self.server_context.access_token_endpoint, 'POST', headers=self._base_header, body=params)

        response = requests.post(uri, headers=headers, data=body)
        self._check_response(response)
        self._oauth_token, self._oauth_token_secret = self._parse_oauth_response(response)
        self._update_client()

    def set_oauth_request_token(self):
        logger.debug('Fetching oauth request token from: %s',
                      self.server_context.request_token_endpoint)
        token_client = oauth.Client(self.consumer_key, client_secret=self.consumer_secret)
        uri, headers, body = token_client.sign(
            self.server_context.request_token_endpoint, "POST")
        response = requests.post(uri, headers=headers, data=body)
        self._check_response(response)
        self._oauth_token , self._oauth_token_secret = self._parse_oauth_response(response)

    def set_oauth_access_token(self):
        logger.debug('Fetching oauth access token from %s',
                      self.server_context.access_token_endpoint)
        access_client = oauth.Client(self.consumer_key,
                                     client_secret=self.consumer_secret,
                                     resource_owner_key=self._oauth_token,
                                     resource_owner_secret=self._oauth_token_secret)
        uri, headers, body = access_client.sign(
            self.server_context.access_token_endpoint, "POST")
        access_response = requests.post(uri, headers=headers, data=body)
        self._check_response(access_response)
        self._oauth_token, self._oauth_token_secret = self._parse_oauth_response(access_response)

    def auth(self):
         self.set_oauth_request_token()
         self.set_oauth_access_token()
         self._update_client()

