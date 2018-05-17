# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.six import string_types

__all__ = [
    'Server',
    'WolframPublicCloudServer'
    ]

class Server(object):
    ''' Represents the cloud server.

    Contains the authentication endpoints informations, the API endpoint aka. the
    cloud base (`$CloudBase` in the Wolfram Language), and eventually the xauth
    consumer key and secret.

    For conveniency this class exposes static methods to build instances from
    a `Configuration` or from a file path.
    '''
    __slots__ = 'cloudbase', 'request_token_endpoint', 'access_token_endpoint', 'xauth_consumer_key', 'xauth_consumer_secret', 'certificate'

    def __init__(self, cloudbase, request_token_endpoint, access_token_endpoint,
                 xauth_consumer_key=None, xauth_consumer_secret=None, certificate=None):
        self.cloudbase = cloudbase
        self.request_token_endpoint = request_token_endpoint
        self.access_token_endpoint = access_token_endpoint
        self.xauth_consumer_key = xauth_consumer_key
        self.xauth_consumer_secret = xauth_consumer_secret
        self.certificate = certificate

    def is_xauth(self):
        return self.xauth_consumer_key is not None and self.xauth_consumer_secret is not None

    @property
    def verify(self):
        if self.certificate is None:
            return True
        elif isinstance(self.certificate, string_types):
            return self.certificate
        else:
            raise ValueError('Invalid certificate. Must be a string type or None.')

# A built-in instance representing the Wolfram public Cloud.
WolframPublicCloudServer = Server(
    'https://www.wolframcloud.com',
    'https://account.wolfram.com/auth/request-token',
    'https://account.wolfram.com/auth/access-token',
    xauth_consumer_key='tbd',
    xauth_consumer_secret='tbd'
)
