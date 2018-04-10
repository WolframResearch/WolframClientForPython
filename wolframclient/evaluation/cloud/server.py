from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.evaluation.configuration import WolframPublicCloudConfig, server_configuration

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
        ''' Build a server instance from a `Configuration`.'''
        return Server(config.api_api_endpoint, 
                      config.authentication_request_token_endpoint,
                      config.authentication_access_token_endpoint,
                      xauth_consumer_key=config.authentication_xauth_consumer_key,
                      xauth_consumer_secret=config.authentication_xauth_consumer_secret)

    @staticmethod
    def from_file(filepath):
        ''' Build a server instance from a file path.'''
        return Server.from_config(server_configuration().read(filepath))
    
    @staticmethod
    def default():
        ''' A new Server instance representing the Wolfram public Cloud.'''
        return Server.from_config(WolframPublicCloudConfig)


# A built-in instance representing the Wolfram public Cloud.
WolframPublicCloudServer = Server.default()
