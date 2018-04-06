from __future__ import absolute_import, print_function, unicode_literals
import logging
from wolframclient.evaluation.cloud.exceptions import AuthenticationException, XAuthNotConfigured, InputException
from wolframclient.evaluation.cloud.oauth import OAuthSession
from wolframclient.evaluation.cloud.inputoutput import WolframAPIResponse, WolframAPI
from wolframclient.evaluation.cloud.server import WolframPublicCloudServer
from wolframclient.utils.encoding import force_text
import requests

__all__ = ['CloudSession']

logger = logging.getLogger(__name__)
class CloudSession(object):
    __slots__ = 'server', 'oauth', 'consumer', 'consumer_secret', 'user', 'password', 'is_xauth'
    def __init__(self, server):
        self.server = server
        self.oauth = None
        self.consumer = None
        self.consumer_secret = None
        self.user = None
        self.password = None
        self.is_xauth = None

    @staticmethod
    def default():
        return CloudSession(WolframPublicCloudServer)

    def authenticate(self, credential):
        if credential.is_xauth:
            self.user_authentication(credential)
        else:
            self.sak_authentication(credential)

    def sak_authentication(self, sak_credential):
        self.consumer = sak_credential.consumer_key
        self.consumer_secret = sak_credential.consumer_secret
        self.is_xauth = False
        self.oauth = OAuthSession(self.consumer, self.consumer_secret)
        self.oauth.auth()

    
    def user_authentication(self, user_credentials):
        if not self.server.is_xauth():
            raise XAuthNotConfigured
        self.user = user_credentials.user
        self.password = user_credentials.password
        self.is_xauth = True
        self.oauth = OAuthSession(self.server.xauth_consumer_key,
                           self.server.xauth_consumer_secret)
        self.oauth.xauth(user_credentials.user, user_credentials.password)
        

    def _auth(self):
        if self.is_xauth is None:
            raise AuthenticationException('Credentials not set.')
        elif self.is_xauth:
            self.oauth.xauth(self.user, self.password)
        else:
            self.oauth.auth()

    @property
    def authorized(self):
        if self.is_xauth is None or self.oauth is None:
            return False
        else:
            return bool(self.oauth._client.client_secret) and bool(self.oauth._client.resource_owner_key) and bool(self.oauth._client.resource_owner_secret)

    def _encoded_inputs(self, inputs, encoders):
        if encoders is None:
            return inputs
        elif callable(encoders):
            inputs_encoded = {}
            for name, value in inputs.items():
                inputs_encoded[name] = encoders(value)
            return inputs_encoded
        elif isinstance(encoders, dict):
            if len(inputs) == 0:
                return {}
            inputs_encoded = {}
            for input_name, input_value in inputs.items():
                if input_name in encoders:
                    inputs_encoded[input_name] = encoders[input_name][input_value]
                else:
                    inputs_encoded[input_name] = input_value
                return inputs_encoded
        else:
            raise InputException("Invalid input encoders. Expecting None, a callable object or a dictionary.")


    def call(self, url, session, input_parameters={}, input_encoders={}, decoder=None):
        encoded_inputs = self._encoded_inputs(input_parameters, input_encoders)
        if self.authorized:
            logger.debug('Authenticated call to api %s', url)
            request = self.oauth.signed_request(url, body=encoded_inputs)
        else:
            logger.debug('Anonymous call to api %s', url)
            request = requests.post(url, data=encoded_inputs)
        return WolframAPIResponse(request, decoder=decoder)


class APIUtil(object):
    @staticmethod
    def user_api_url(username, api_id, 
        server=WolframPublicCloudServer,
        input_encoders=None, 
        decoder=force_text):
        '''Build an API URL from a user name and an API id.
        
        user name is generally $UserName. API id can be a uuid or a name,
        in the form of a relative path. e.g: myapi/foo/bar
        '''
        return URLBuilder(server.cloudbase).extend('objects', username, api_id).get()

    def buildin_api(self, name):
        '''Returns a build-in Wolfram API. '''
        raise NotImplementedError('Not supported yet.')

class URLBuilder(object):
    ''' Very basic mutable string builder that only ensures consistent slashes.'''
    __slots__ = 'parts'

    def __init__(self, base=""):
        self.parts = [base]

    def extend(self, *fragments):
        for fragment in fragments:
            self.append(fragment)
        return self

    def append(self, fragment):
        last_fragment = self.parts[-1]
        if last_fragment.endswith('/'):
            if fragment.startswith('/'):
                self.parts.append(fragment[1:])
            else:
                self.parts.append(fragment)
        else:
            if len(last_fragment) > 0:
                if not fragment.startswith('/'):
                    self.parts.append('/')
                self.parts.append(fragment)
            elif fragment.startswith('/'):
                self.parts.append(fragment[1:])
            else:
                self.parts.append(fragment)
        return self

    def get(self):
        return "".join(self.parts)
