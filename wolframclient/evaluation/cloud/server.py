from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.evaluation.configuration import WolframPublicCloudConfig

__all__ = [
    'Server', 
    'WolframPublicCloudServer'
    ]

class Server(object):

    __slots__ = 'cloudbase', 'request_token_endpoint', 'access_token_endpoint', 'xauth_consumer_key', 'xauth_consumer_secret'

    def __init__(self, cloudbase, request_token_endpoint, access_token_endpoint,
        xauth_consumer_key=None, xauth_consumer_secret=None):
        self.cloudbase = cloudbase
        self.request_token_endpoint = request_token_endpoint
        self.access_token_endpoint = access_token_endpoint
        self.xauth_consumer_key = xauth_consumer_key
        self.xauth_consumer_secret = xauth_consumer_secret
    
    def is_xauth(self):
        return self.xauth_consumer_key is not None and self.xauth_consumer_secret is not None

    @staticmethod
    def from_config(config):
        return Server(config.api_api_endpoint, 
                      config.authentication_request_token_endpoint,
                      config.authentication_access_token_endpoint,
                      xauth_consumer_key=config.authentication_xauth_consumer_key,
                      xauth_consumer_secret=config.authentication_xauth_consumer_secret)


WolframPublicCloudServer = Server.from_config(WolframPublicCloudConfig)
