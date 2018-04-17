# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
import os
import logging
try: # PY3
    from urllib.parse import urlparse, parse_qs, urlencode, quote_plus
except ImportError: # PY2
    from urlparse import urlparse, parse_qs
    from urllib import urlencode, quote_plus
    
import requests
from oauthlib import oauth1 as oauth

from wolframclient.evaluation.cloud.server import WolframPublicCloudServer
from wolframclient.evaluation.configuration import user_credential_configuration, sak_configuration
from wolframclient.evaluation.cloud.exceptions import RequestException, AuthenticationException, XAuthNotConfigured, ConfigurationException
from wolframclient.utils.six import string_types, binary_type

logger = logging.getLogger(__name__)

class SecuredAuthenticationKey(object):
    ''' Represents a Secured Authentication Key generated using the Wolfram Language
    function `GenerateSecuredAuthenticationKey[]`
    
    It is used as an input when authenticating a cloud session. This class provides
    static builder methods:
    - from a file path with `from_file`
    - from an instance of `Configuration` with `from_config`
    '''
    __slots__ = 'consumer_key', 'consumer_secret'
    is_xauth = False
    def __init__(self, consumer_key, consumer_secret):
        self.consumer_key = consumer_key
        self.consumer_secret = consumer_secret

    @staticmethod
    def from_config(config):
        ''' Build a new instance of `SecuredAuthenticationKey` from a given configuration.

        A convenient way to get an appropriate configuration instance is to use
        from `wolframclient.evaluation.configuration.sak_configuration()`
        '''
        if hasattr(config, 'sak_consumer_key') and hasattr(config, 'sak_consumer_secret'):
            return SecuredAuthenticationKey(config.sak_consumer_key, config.sak_consumer_secret)
        else:
            raise ConfigurationException(
                'Consumer key and secret missing from configuration.')

    @staticmethod
    def from_file(filepath):
        ''' Build a new instance of `SecuredAuthenticationKey` from a given configuration file'''
        return SecuredAuthenticationKey.from_config(
            sak_configuration().read(filepath)
        )

class UserCredentials(object):
    ''' Represents user credentials used to login to a cloud.
    
    It is used as an input when authenticating a cloud session. This class provides 
    static builder methods:
    - from a file path with `from_file`
    - from an instance of `Configuration` with `from_config`
    '''
    __slots__ = 'user', 'password'
    is_xauth = True
    def __init__(self, user, password):
        self.user = user
        self.password = password

    @staticmethod
    def from_config(config):
        ''' Build a new instance of `UserCredentials` from a given configuration.

        A convenient way to get an appropriate configuration instance is to use
        from `wolframclient.evaluation.configuration.user_credential_configuration()`
        '''
        if hasattr(config, 'user_id') and hasattr(config, 'user_password'):
            return UserCredentials(config.user_id, config.user_password)
        else:
            raise ConfigurationException('User credentials missing from configuration.')

    @staticmethod
    def from_file(filepath):
        ''' Build a new instance of `UserCredentials` from a given configuration file'''
        return UserCredentials.from_config(
            user_credential_configuration().read(filepath)
        )

class OAuthSession(object):
    ''' A wrapper around the OAuth client taking care of fetching the various oauth tokens.

    This class is used by the cloud session. It is not meant to be used out of this scope.
    '''
    __slots__ = 'consumer_key', 'consumer_secret', 'signature_method',  '_oauth_token', '_oauth_token_secret', '_base_header', '_client', 'server', '_session'
    DEFAULT_CONTENT_TYPE = {
        'Content-Type': 'application/x-www-form-urlencoded',
        'User-Agent': 'WolframClientForPython/1.0'}

    def __init__(self, server, consumer_key, consumer_secret, signature_method=oauth.SIGNATURE_HMAC):
        self.consumer_key = consumer_key
        self.consumer_secret = consumer_secret
        self.signature_method = signature_method
        self._client = None
        self._session = None
        self._oauth_token = None
        self._oauth_token_secret = None
        self.server = server
    
    def _check_response(self, response):
        # TODO: deal with error code with more precision.
        logger.debug('[Auth] Code: %i\nBody: %s', response.status_code, response.text)
        if response.status_code != 200:
            raise AuthenticationException(response)

    def _update_client(self):
        self._client = oauth.Client(self.consumer_key,
                                    client_secret=self.consumer_secret,
                                    resource_owner_key=self._oauth_token,
                                    resource_owner_secret=self._oauth_token_secret,
                                    signature_type=oauth.SIGNATURE_TYPE_AUTH_HEADER,
                                    realm=self.server.cloudbase) #TODO should we have realm?

    @staticmethod
    def _has_content_type(request, mimetype):
        return request.headers.get('Content-Type', None) == mimetype

    @staticmethod
    def _is_json_content(request):
        return OAuthSession._has_content_type(request, 'application/json')
    
    @staticmethod
    def _is_textplain_content(request):
        return OAuthSession._has_content_type(request, 'text/plain')

    # TODO Add a session and prepared requests?
    def signed_request(self, uri, headers={}, body={}, method='POST'):
        if self._client is None:
            raise AuthenticationException('Not authenticated.')
        req_headers = headers
        req_headers['User-Agent'] = 'WolframClientForPython/1.0'
        if isinstance(body, dict):
            # url encode the body
            encoded_body = urlencode(body)
            req_headers['Content-Type'] = 'application/x-www-form-urlencoded'
        elif isinstance(body, string_types) or isinstance(body, binary_type):
            encoded_body = body
            if 'Content-Type' not in req_headers:
                req_headers['Content-Type'] = 'text/plain'
        else:
            raise ValueError('Headers must be dict or string type.')
        
        uri, req_headers, body = self._client.sign(
            uri,
            method,
            body=encoded_body,
            headers=req_headers,
            realm=self.server.cloudbase  # TODO should we have realm?
        )
        return requests.request(method, uri, headers=req_headers, data=body, verify=self.server.verify)

    @property
    def authorized(self):
        return self._client is not None and bool(self._client.client_secret) and bool(self._client.resource_owner_key) and bool(self._client.resource_owner_secret)

    def _parse_oauth_response(self, response):
        if OAuthSession._is_json_content(response):
            from json import loads as json_loads
            token = json_loads(response.text)
            return token['oauth_token'], token['oauth_token_secret']
        elif not OAuthSession._is_textplain_content(response):
            logging.warning('Unexpected content type in oauth response. Parsing as query string.')
            
        token = parse_qs(response.text)
        return (token.get('oauth_token')[0],
            token.get('oauth_token_secret')[0])

    def xauth(self, user, password):
        logger.debug('xauth authentication of user %s', user)
        if not self.server.is_xauth():
            raise XAuthNotConfigured
        #todo use xauth server key/secret
        client = oauth.Client(
            self.consumer_key, self.consumer_secret)
        params = {}
        params["x_auth_username"] = user
        params["x_auth_password"] = password
        params["x_auth_mode"] = "client_auth"
        
        logging.disable(logging.DEBUG)
        
        uri, headers, body = client.sign(
            self.server.access_token_endpoint, 'POST', headers=OAuthSession.DEFAULT_CONTENT_TYPE, body=params)

        response = requests.post(uri, headers=headers, data=body, verify=self.server.verify)

        logging.disable(logging.NOTSET)

        self._check_response(response)
        self._oauth_token, self._oauth_token_secret = self._parse_oauth_response(response)
        self._update_client()

    def set_oauth_request_token(self):
        logger.debug('Fetching oauth request token from: %s',
                      self.server.request_token_endpoint)
        
        logging.disable(logging.DEBUG)

        token_client = oauth.Client(self.consumer_key, client_secret=self.consumer_secret)
        uri, headers, body = token_client.sign(
            self.server.request_token_endpoint, "POST")
        response = requests.post(uri, headers=headers,
                                 data=body, verify=self.server.verify)

        logging.disable(logging.NOTSET)

        self._check_response(response)
        self._oauth_token , self._oauth_token_secret = self._parse_oauth_response(response)

    def set_oauth_access_token(self):
        logger.debug('Fetching oauth access token from %s',
                      self.server.access_token_endpoint)
        access_client = oauth.Client(self.consumer_key,
                                     client_secret=self.consumer_secret,
                                     resource_owner_key=self._oauth_token,
                                     resource_owner_secret=self._oauth_token_secret)
        uri, headers, body = access_client.sign(
            self.server.access_token_endpoint, "POST")
        access_response = requests.post(
            uri, headers=headers, data=body, verify=self.server.verify)
        self._check_response(access_response)
        self._oauth_token, self._oauth_token_secret = self._parse_oauth_response(access_response)

    def auth(self):
         self.set_oauth_request_token()
         self.set_oauth_access_token()
         self._update_client()

