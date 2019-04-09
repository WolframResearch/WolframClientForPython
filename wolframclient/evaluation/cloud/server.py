# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six

__all__ = ['WolframServer']


class WolframServer(object):
    ''' Represents the cloud server.

    Contains the authentication endpoints information, the API endpoint aka. the
    cloud base (`$CloudBase` in the Wolfram Language), and eventually the xauth
    consumer key and secret.
    '''

    def __init__(self,
                 cloudbase,
                 request_token_endpoint,
                 access_token_endpoint,
                 xauth_consumer_key=None,
                 xauth_consumer_secret=None,
                 certificate=None):
        self.cloudbase = cloudbase
        self.request_token_endpoint = request_token_endpoint
        self.access_token_endpoint = access_token_endpoint
        self.xauth_consumer_key = xauth_consumer_key
        self.xauth_consumer_secret = xauth_consumer_secret
        if certificate is None or isinstance(certificate, six.string_types):
            self.certificate = certificate
        else:
            raise ValueError(
                'Invalid certificate. Must be a string type or None.')

    def is_xauth(self):
        return self.xauth_consumer_key is not None and self.xauth_consumer_secret is not None

    def __repr__(self):
        return '<%s: cloudbase=%s, request_token=%s, access_token=%s, certificate=%s, xauth support=%s>' % (
            self.__class__.__name__, self.cloudbase,
            self.request_token_endpoint, self.access_token_endpoint,
            self.certificate, self.is_xauth())


# A built-in instance representing the Wolfram Public Cloud.
WOLFRAM_PUBLIC_CLOUD_SERVER = WolframServer(
    'https://www.wolframcloud.com',
    'https://account.wolfram.com/auth/request-token',
    'https://account.wolfram.com/auth/access-token',
    xauth_consumer_key=None,
    xauth_consumer_secret=None)
