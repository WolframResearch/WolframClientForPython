# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.exception import AuthenticationException
from wolframclient.utils import six
from wolframclient.utils.api import json, oauth, requests, urllib

import logging

logger = logging.getLogger(__name__)


__all__ = ['SecuredAuthenticationKey', 'UserIDPassword']

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

class OAuthSession(object):
    ''' A wrapper around the OAuth client taking care of fetching the various oauth tokens.

    This class is used by the cloud session. It is not meant to be used out of this scope.
    '''
    __slots__ = 'consumer_key', 'consumer_secret', 'signature_method',  '_oauth_token', '_oauth_token_secret', '_base_header', '_client', 'server', '_session'
    DEFAULT_CONTENT_TYPE = {
        'Content-Type': 'application/x-www-form-urlencoded',
        'User-Agent': 'WolframClientForPython/1.0'}

    def __init__(self, server, consumer_key, consumer_secret, signature_method=None):
        self.consumer_key = consumer_key
        self.consumer_secret = consumer_secret
        self.signature_method = signature_method or oauth.SIGNATURE_HMAC
        self._client = None
        self._session = None
        self._oauth_token = None
        self._oauth_token_secret = None
        self.server = server

    def _check_response(self, response):
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('[Auth] Code: %i\nBody: %s', response.status_code, response.text)
        if response.status_code == 200:
            return
        if response.status_code == 400:
            try:
                as_json = response.json()
                msg = as_json.get('message', None)
            except:
                pass
        raise AuthenticationException(response, msg)

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

        req_headers={}
        for k,v in headers.items():
            req_headers[k] = v
        if isinstance(body, dict):
            # url encode the body
            encoded_body = urllib.urlencode(body)
            sign_body = True
            if 'Content-Type' not in req_headers:
                logger.info('Content type not provided by user. Setting it to "application/x-www-form-urlencoded".')
                req_headers['Content-Type'] = 'application/x-www-form-urlencoded'
        elif isinstance(body, six.string_types) or isinstance(body, six.binary_type):
            # application/octet-stream for binary data?
            if 'Content-Type' not in req_headers:
                logger.info('Content type not provided by user. Setting it to "text/plain".')
                req_headers['Content-Type'] = 'text/plain'
                sign_body = False
            elif 'application/x-www-form-urlencoded' == req_headers['Content-Type']:
                encoded_body = body
                sign_body = True
            else:
                sign_body = False
        else:
            logger.fatal('Invalid body: %s', body)
            raise ValueError('Body must be dict or string type.')

        uri, req_headers, signed_body = self._client.sign(
            uri,
            method,
            body= encoded_body if sign_body else None,
            headers=req_headers,
            realm=self.server.cloudbase  # TODO should we have realm?
        )
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Signed uri: %s', uri)
            logger.debug('Signed header: %s', req_headers)
            logger.debug('Signed body: %s', sign_body)

        return requests.request(method, uri,
            headers=req_headers,
            data=signed_body if sign_body else body,
            verify=self.server.verify)

    @property
    def authorized(self):
        return self._client is not None and bool(self._client.client_secret) and bool(self._client.resource_owner_key) and bool(self._client.resource_owner_secret)

    def _parse_oauth_response(self, response):
        if OAuthSession._is_json_content(response):

            token = json.loads(response.text)
            return token['oauth_token'], token['oauth_token_secret']
        elif not OAuthSession._is_textplain_content(response):
            logger.warning('Unexpected content type in oauth response. Parsing as query string.')

        token = urllib.parse_qs(response.text)
        return (token.get('oauth_token')[0],
            token.get('oauth_token_secret')[0])

    def xauth(self, user, password):
        logger.debug('xauth authentication of user %s', user)
        if not self.server.is_xauth():
            raise AuthenticationException('XAuth is not configured. Missing consumer key and/or secret.')
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