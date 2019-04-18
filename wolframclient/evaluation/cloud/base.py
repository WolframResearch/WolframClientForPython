# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import json
from io import IOBase

from wolframclient.utils import six
from wolframclient.utils.api import oauth, urllib

__all__ = [
    'SecuredAuthenticationKey', 'UserIDPassword', 'OAuthSessionBase',
    'OAuthAsyncSessionBase'
]


class SecuredAuthenticationKey(object):
    ''' Represents a Secured Authentication Key generated using the Wolfram Language
    function `GenerateSecuredAuthenticationKey[]`

    It is used as an input when authenticating a cloud session.
    '''
    is_xauth = False

    def __init__(self, consumer_key, consumer_secret):
        self.consumer_key = consumer_key
        self.consumer_secret = consumer_secret


class UserIDPassword(object):
    ''' Represents user credentials used to login to a cloud.

    It is used as an input when authenticating a cloud session.
    '''
    is_xauth = True

    def __init__(self, user, password):
        self.user = user
        self.password = password


class OAuthSessionBase(object):
    """ A family of classes dealing with authentication with OAuth method."""
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

    def authenticate(self):
        """ Authenticate with a given server using the user credentials"""
        raise NotImplementedError

    def signed_request(self, uri, headers={}, data=None, method='POST'):
        """ Sign a given request and issue it."""
        raise NotImplementedError

    def authorized(self):
        """Return a reasonably accurate state of the authentication status."""
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


class OAuthAsyncSessionBase(OAuthSessionBase):
    async def authenticate(self):
        """ Asynchronous OAuth authentication class dealing with various tokens and signing requests. """
        raise NotImplementedError

    async def signed_request(self, uri, headers={}, data=None, method='POST'):
        """ Sign a given request and issue it asynchronously."""
        raise NotImplementedError


class WolframAPICallBase(object):
    """Perform an API call to a given target.

    The API call is actually performed when :func:`~wolframclient.evaluation.WolframAPICall.perform`
    is called.

    Parameters can be added using one of the various functions that this class exposes. They
    can be of many types including: string, files, WL serializable python objects, binary data with arbitrary
    content-type (e.g: *image/png*).
    """

    def __init__(self, target, api, permission_key=None):
        self.target = target
        self.api = api
        self.parameters = {}
        self.files = {}
        self.permission_key = permission_key

    def set_parameter(self, name, value):
        """Add a new API input parameter from a serializable python object."""
        self.parameters[name] = value
        return self

    def add_file_parameter(self, name, fp, filename=None, content_type=None):
        """Add a new API input parameter from a file pointer `fp`"""
        if content_type is None:
            self.files[name] = fp
        else:
            fname = filename or 'tmp_%s' % name
            self.files[name] = (fname, fp, content_type)
        return self

    def add_binary_parameter(self,
                             name,
                             data,
                             filename=None,
                             content_type='application/octet-stream'):
        """Add a new API input parameter as a blob of binary data."""
        if isinstance(data, IOBase):
            return self.add_file_parameter(
                name, data, filename=filename, content_type=content_type)
        if isinstance(data, six.binary_type):
            fname = filename or 'tmp_%s' % name
            self.files[name] = (fname, data, content_type)
            return self
        else:
            raise TypeError('Input data must be bytes or IOBase.')

    def add_image_data_parameter(self,
                                 name,
                                 image_data,
                                 filename=None,
                                 content_type='image/png'):
        """Add a new API image input parameter from binary data.

        If the data in `image_data` does not represent an image in the `PNG` format, the optional parameter
        `content_type` must be set accordingly to the appropriate content type, e.g. *image/jpeg*, *image/gif*, etc.
        """
        return self.add_binary_parameter(name, image_data, filename,
                                         content_type)

    def perform(self, **kwargs):
        """Make the API call and return the result."""
        raise NotImplementedError

    def perform_future(self, **kwargs):
        """Make the API call asynchronously and return a future object."""
        raise NotImplementedError

    def __repr__(self):
        return '<%s api=%s, parameters=%s>' % (
            self.__class__.__name__, self.api,
            set().union(self.parameters.keys(), self.files.keys()) or None)

    def __str__(self):
        return repr(self)
