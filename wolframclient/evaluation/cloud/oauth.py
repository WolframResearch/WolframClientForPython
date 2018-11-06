# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
from wolframclient.evaluation.cloud.base import (
    UserIDPassword, OAuthSessionBase, OAuthSyncSessionBase)
from wolframclient.exception import AuthenticationException
from wolframclient.utils import six
from wolframclient.utils.api import oauth, requests, urllib

logger = logging.getLogger(__name__)

__all__ = ['OAuth1RequestsSyncSession', 'XAuthRequestsSyncSession']

class OAuthRequestsSyncSessionBase(OAuthSyncSessionBase):
    ''' A wrapper around the OAuth client taking care of fetching the various oauth tokens.

    This class is used by the cloud session. It is not meant to be used out of this scope.
    '''

    def __init__(self,
                 server,
                 consumer_key,
                 consumer_secret,
                 signature_method=None,
                 client_class=oauth.Client):
        super().__init__(server, consumer_key, consumer_secret, signature_method=signature_method, client_class=client_class)
        self.verify = self.server.certificate
    # __slots__ = 'consumer_key', 'consumer_secret', 'signature_method', '_oauth_token', '_oauth_token_secret', '_base_header', '_client', 'server'
    # DEFAULT_CONTENT_TYPE = {
    #     'Content-Type': 'application/x-www-form-urlencoded',
    #     'User-Agent': 'WolframClientForPython/1.0'
    # }

    # def __init__(self,
    #              server,
    #              consumer_key,
    #              consumer_secret,
    #              signature_method=None,
    #              client_class=self.client_class):
    #     # self.consumer_key = consumer_key
    #     # self.consumer_secret = consumer_secret
    #     # self.signature_method = signature_method or oauth.SIGNATURE_HMAC
    #     # self._client = None
    #     # self._oauth_token = None
    #     # self._oauth_token_secret = None
    #     # self.server = server

    def _check_response(self, response):
        msg = None
        if response.status_code == 200:
            return
        try:
            as_json = response.json()
            msg = as_json.get('message', None)
        # msg is None if response is not JSON, but it's fine.
        except:
            raise AuthenticationException(
                response,
                'Request failed with status %i' % response.status_code)
        raise AuthenticationException(response, msg)

    # def _update_client(self):
    #     self._client = self.client_class(
    #         self.consumer_key,
    #         client_secret=self.consumer_secret,
    #         resource_owner_key=self._oauth_token,
    #         resource_owner_secret=self._oauth_token_secret,
    #         signature_type=oauth.SIGNATURE_TYPE_AUTH_HEADER,
    #         realm=self.server.cloudbase,  # TODO should we have realm?
    #         encoding='iso-8859-1')

    # TODO Add a session and prepared requests?
    def signed_request(self, uri, headers={}, body={}, files={},
                       method='POST'):
        if not self.authorized():
            self.authenticate()
        
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
            verify=self.verify)

    # @property
    # def authorized(self):
    #     return self._client is not None and bool(
    #         self._client.client_secret) and bool(
    #             self._client.resource_owner_key) and bool(
    #                 self._client.resource_owner_secret)

    # def _parse_oauth_response(self, response):
    #     try:
    #         token = response.json()
    #         return token['oauth_token'], token['oauth_token_secret']
    #     except:
    #         token = urllib.parse_qs(response.text)
    #         return (token.get('oauth_token')[0],
    #                 token.get('oauth_token_secret')[0])

        

class OAuth1RequestsSyncSession(OAuthRequestsSyncSessionBase):

    def authenticate(self):
        self.set_oauth_request_token()
        self.set_oauth_access_token()
        self._update_client()
    
    def set_oauth_request_token(self):
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Fetching oauth request token from: %s',
                         self.server.request_token_endpoint)

        logging.disable(logging.DEBUG)

        token_client = self.client_class(
            self.consumer_key, client_secret=self.consumer_secret)
        uri, headers, body = token_client.sign(
            self.server.request_token_endpoint, "POST")
        response = requests.post(
            uri, headers=headers, data=body, verify=self.verify)

        logging.disable(logging.NOTSET)

        self._check_response(response)
        self._update_token_from_request_body(response.content)

    def set_oauth_access_token(self):
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
        access_response = requests.post(
            uri, headers=headers, data=body, verify=self.verify)
        
        self._check_response(access_response)
        self._update_token_from_request_body(access_response.content)


class XAuthRequestsSyncSession(OAuthRequestsSyncSessionBase):

    def __init__(self, userid_password, server, consumer_key, consumer_secret, signature_method=None, client_class=oauth.Client):
        super().__init__(server, server.xauth_consumer_key,
            server.xauth_consumer_secret, 
            signature_method=signature_method, client_class=client_class)
        if not self.server.is_xauth():
            raise AuthenticationException(
                'XAuth is not configured for this server. Missing xauth consumer key and/or secret.')
        if isinstance(userid_password, tuple) and len(userid_password) == 2:
            self.xauth_credentials = UserIDPassword(*userid_password)
        elif isinstance(userid_password, UserIDPassword):
            self.xauth_credentials = userid_password
        else:
            raise ValueError('User ID and password must be specified as a tuple or a UserIDPassword instance.')
        
    def authenticate(self):
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('xauth authentication of user %s', user)
        if not self.server.is_xauth():
            raise AuthenticationException(
                'XAuth is not configured. Missing consumer key and/or secret.')
        #todo use xauth server key/secret
        client = self.client_class(self.consumer_key, self.consumer_secret)
        params = {}
        params["x_auth_username"] = self.xauth_credentials.user
        params["x_auth_password"] = self.xauth_credentials.password
        params["x_auth_mode"] = "client_auth"

        # avoid dumping password in log files.
        logging.disable(logging.DEBUG)

        uri, headers, body = client.sign(
            self.server.access_token_endpoint,
            'POST',
            headers=self.DEFAULT_CONTENT_TYPE,
            body=params)

        response = requests.post(
            uri, headers=headers, data=body, verify=self.verify)

        logging.disable(logging.NOTSET)

        self._check_response(response)
        self._update_token_from_request_body(response.content)
        self._update_client()