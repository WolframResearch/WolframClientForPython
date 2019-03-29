# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging

from wolframclient.evaluation.cloud.base import (OAuthSessionBase,
                                                 UserIDPassword)
from wolframclient.exception import AuthenticationException
from wolframclient.utils import six
from wolframclient.utils.api import oauth, urllib

logger = logging.getLogger(__name__)

__all__ = ['OAuth1RequestsSyncSession', 'XAuthRequestsSyncSession']


class OAuthRequestsSyncSessionBase(OAuthSessionBase):
    """ A wrapper around the OAuth client taking care of fetching the various oauth tokens,
    preparing data as expected by the requests library.
    """

    def __init__(self,
                 http_session,
                 server,
                 consumer_key,
                 consumer_secret,
                 signature_method=None,
                 client_class=None):
        super().__init__(
            server,
            consumer_key,
            consumer_secret,
            signature_method=signature_method,
            client_class=client_class or oauth.Client)
        self.http_session = http_session
        self.verify = self.server.certificate

    def _check_response(self, response):
        msg = None
        if response.status_code == 200:
            return
        try:
            as_json = response.json()
            msg = as_json.get('message', None)
            raise AuthenticationException(response, msg)
        # msg is None if response is not JSON, but it's fine.
        except Exception:
            msg = 'Request failed with status %i' % response.status_code
            raise AuthenticationException(response, msg=msg)

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
            realm=self.server.cloudbase)
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Signed uri: %s', uri)
            logger.debug('Signed header: %s', req_headers)
            logger.debug('Is body signed: %s', sign_body)

        return self.http_session.request(
            method,
            uri,
            headers=req_headers,
            data=signed_body if sign_body else body,
            files=files,
            verify=self.verify)


class OAuth1RequestsSyncSession(OAuthRequestsSyncSessionBase):
    """ Oauth1 authentication using secured authentication key, as expected by the requests library. """

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
        response = self.http_session.post(
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
        access_response = self.http_session.post(
            uri, headers=headers, data=body, verify=self.verify)

        self._check_response(access_response)
        self._update_token_from_request_body(access_response.content)


class XAuthRequestsSyncSession(OAuthRequestsSyncSessionBase):
    """ XAuth authentication as expected by the requests library. 
    
    xauth authenticates with user and password, but requires a specific server
    configuration. """

    def __init__(self,
                 userid_password,
                 http_session,
                 server,
                 consumer_key,
                 consumer_secret,
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

        response = self.http_session.post(
            uri, headers=headers, data=body, verify=self.verify)

        logging.disable(logging.NOTSET)

        self._check_response(response)
        self._update_token_from_request_body(response.content)
        self._update_client()
