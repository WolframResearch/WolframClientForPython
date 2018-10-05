# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging

from wolframclient.exception import AuthenticationException
from wolframclient.utils import six
from wolframclient.utils.api import oauth, requests, urllib

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
    __slots__ = 'consumer_key', 'consumer_secret', 'signature_method', '_oauth_token', '_oauth_token_secret', '_base_header', '_client', 'server', '_session'
    DEFAULT_CONTENT_TYPE = {
        'Content-Type': 'application/x-www-form-urlencoded',
        'User-Agent': 'WolframClientForPython/1.0'
    }

    def __init__(self,
                 server,
                 consumer_key,
                 consumer_secret,
                 signature_method=None):
        self.consumer_key = consumer_key
        self.consumer_secret = consumer_secret
        self.signature_method = signature_method or oauth.SIGNATURE_HMAC
        self._client = None
        self._session = None
        self._oauth_token = None
        self._oauth_token_secret = None
        self.server = server

    def _check_response(self, response):
        if response.status_code == 200:
            return
        try:
            as_json = response.json()
            msg = as_json.get('message', None)
        # msg is None if response is not JSON, but it's fine.
        except:
            pass
        raise AuthenticationException(response, msg)

    def _update_client(self):
        self._client = oauth.Client(
            self.consumer_key,
            client_secret=self.consumer_secret,
            resource_owner_key=self._oauth_token,
            resource_owner_secret=self._oauth_token_secret,
            signature_type=oauth.SIGNATURE_TYPE_AUTH_HEADER,
            realm=self.server.cloudbase,  # TODO should we have realm?
            encoding='iso-8859-1')

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
    def signed_request(self, uri, headers={}, body={}, files={},
                       method='POST'):
        if self._client is None:
            raise AuthenticationException('Not authenticated.')

        req_headers = {}
        for k, v in headers.items():
            req_headers[k] = v
        sign_body = False
        #if files is set, it's a multipart request.
        if len(files) == 0:
            if isinstance(body, dict):
                # url encode the body
                encoded_body = urllib.urlencode(body)
                sign_body = True
                if 'Content-Type' not in req_headers:
                    logger.info(
                        'Content type not provided by user. Setting it to "application/x-www-form-urlencoded".'
                    )
                    req_headers[
                        'Content-Type'] = 'application/x-www-form-urlencoded'
            elif isinstance(body, six.string_types) or isinstance(
                    body, six.binary_type):
                # application/octet-stream for binary data?
                if 'Content-Type' not in req_headers:
                    logger.info(
                        'Content type not provided by user. Setting it to "text/plain".'
                    )
                    req_headers['Content-Type'] = 'text/plain'
                    sign_body = False
                elif 'application/x-www-form-urlencoded' == req_headers[
                        'Content-Type']:
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
            body=encoded_body if sign_body else None,
            headers=req_headers,
            realm=self.server.cloudbase  # TODO should we have realm?
        )
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Signed uri: %s', uri)
            logger.debug('Signed header: %s', req_headers)
            logger.debug('Is body signed: %s', sign_body)

            # s = requests.Session()
            # req = requests.Request(method, uri,
            #                  headers=req_headers,
            #                  data=signed_body if sign_body else body,
            #                  files=files,
            #                  )
            # prepared = req.prepare()
            # logger.debug('Prepared request (generated only in debug)\nheaders: %s\nbody: %s', prepared.headers, prepared.body)
            # return s.send(prepared, verify=self.server.verify)

        return requests.request(
            method,
            uri,
            headers=req_headers,
            data=signed_body if sign_body else body,
            files=files,
            verify=self.server.verify)

    @property
    def authorized(self):
        return self._client is not None and bool(
            self._client.client_secret) and bool(
                self._client.resource_owner_key) and bool(
                    self._client.resource_owner_secret)

    def _parse_oauth_response(self, response):
        try:
            token = response.json()
            return token['oauth_token'], token['oauth_token_secret']
        except:
            token = urllib.parse_qs(response.text)
            return (token.get('oauth_token')[0],
                    token.get('oauth_token_secret')[0])

    def xauth(self, user, password):
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('xauth authentication of user %s', user)
        if not self.server.is_xauth():
            raise AuthenticationException(
                'XAuth is not configured. Missing consumer key and/or secret.')
        #todo use xauth server key/secret
        client = oauth.Client(self.consumer_key, self.consumer_secret)
        params = {}
        params["x_auth_username"] = user
        params["x_auth_password"] = password
        params["x_auth_mode"] = "client_auth"

        # avoid dumping password in log files.
        logging.disable(logging.DEBUG)

        uri, headers, body = client.sign(
            self.server.access_token_endpoint,
            'POST',
            headers=OAuthSession.DEFAULT_CONTENT_TYPE,
            body=params)

        response = requests.post(
            uri, headers=headers, data=body, verify=self.server.verify)

        logging.disable(logging.NOTSET)

        self._check_response(response)
        self._oauth_token, self._oauth_token_secret = self._parse_oauth_response(
            response)
        self._update_client()

    def set_oauth_request_token(self):
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Fetching oauth request token from: %s',
                         self.server.request_token_endpoint)

        logging.disable(logging.DEBUG)

        token_client = oauth.Client(
            self.consumer_key, client_secret=self.consumer_secret)
        uri, headers, body = token_client.sign(
            self.server.request_token_endpoint, "POST")
        response = requests.post(
            uri, headers=headers, data=body, verify=self.server.verify)

        logging.disable(logging.NOTSET)

        self._check_response(response)
        self._oauth_token, self._oauth_token_secret = self._parse_oauth_response(
            response)

    def set_oauth_access_token(self):
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Fetching oauth access token from %s',
                         self.server.access_token_endpoint)
        access_client = oauth.Client(
            self.consumer_key,
            client_secret=self.consumer_secret,
            resource_owner_key=self._oauth_token,
            resource_owner_secret=self._oauth_token_secret)
        uri, headers, body = access_client.sign(
            self.server.access_token_endpoint, "POST")
        access_response = requests.post(
            uri, headers=headers, data=body, verify=self.server.verify)
        self._check_response(access_response)
        self._oauth_token, self._oauth_token_secret = self._parse_oauth_response(
            access_response)

    def auth(self):
        self.set_oauth_request_token()
        self.set_oauth_access_token()
        self._update_client()
