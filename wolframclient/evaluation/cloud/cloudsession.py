from __future__ import absolute_import, print_function, unicode_literals
import logging
from wolframclient.evaluation.cloud.exceptions import AuthenticationException, XAuthNotConfigured, InputException, OutputException
from wolframclient.evaluation.cloud.oauth import OAuthSession
from wolframclient.evaluation.cloud.inputoutput import WolframAPIBuilder
from wolframclient.evaluation.cloud.server import WolframPublicCloudServer
from wolframclient.utils.encoding import force_text
import requests

__all__ = ['CloudSession']

logger = logging.getLogger(__name__)
class CloudSession(object):
    ''' Represents a session to a given cloud enabling simple API call.
    
    This is the central class of the cloud evaluation package. It is 
    initialized with a server instance representing a given cloud. The
    `default` static method can be used to initialize a session to the Wolfram
    public cloud.

    Most of the time it is necessary to authenticate with the server before issuing
    requests. `CloudSession` supports two forms of authentication:
    - 2-legged oauth using a secured authentication key.
    - xauth using the user ID and password.

    Calling an API is done throught the method `call` which will return an instance of
    a `WolframAPIResponse`. It is strongly advised to re-use a session to make multiple 
    calls.
    '''
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
        ''' returns a cloud session targetting the Wolfram public Cloud.'''
        return CloudSession(WolframPublicCloudServer)

    def authenticate(self, credentials):
        '''Authenticate with the server using the credentials. 
        
        This method supports both oauth and xauth methods. '''
        if credentials.is_xauth:
            self.user_authentication(credentials)
        else:
            self.sak_authentication(credentials)
        return self

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
        
    @property
    def authorized(self):
        ''' Returns a reasonnably accurate state of the authentication status. '''
        if self.is_xauth is None or self.oauth is None:
            return False
        else:
            return bool(self.oauth._client.client_secret) and bool(self.oauth._client.resource_owner_key) and bool(self.oauth._client.resource_owner_secret)

    def _encoded_inputs(self, inputs, encoders):
        if isinstance(encoders, dict):
            if len(encoders) == 0:
                return inputs
            inputs_encoded = {}
            for input_name, input_value in inputs.items():
                if input_name in encoders:
                    encoder = encoders[input_name]
                    if callable(encoder):
                        inputs_encoded[input_name] = encoder(input_value)
                    else:
                        raise OutputException('Input encoder of %s is not callable.' % input_name)
                else:
                    inputs_encoded[input_name] = input_value
                return inputs_encoded
        elif callable(encoders):
            inputs_encoded = {}
            for name, value in inputs.items():
                inputs_encoded[name] = encoders(value)
            return inputs_encoded
        else:
            raise InputException("Invalid input encoders. Expecting None, a callable object or a dictionary.")


    def call(self, url, input_parameters={}, input_encoders={}, decoder=force_text):
        ''' Call a given API specified by its `url`, using the provided input parameters
        
        The input parameters are provider as a dictionary with string keys being the name
        of the parameters associated to their value. The input encoder(s) can be specified
        as a callable in which case it is applied to all inputs, or as a dictionary, with keys
        being the parameter names, for a finer control of the encoding. Finally it is possible
        to specify a decoder, which is applied when the request was successful, to the raw binary 
        response of the API. 
        
        Note: By default a decoder is specified and ensure the response is of type string. To
        get raw bytes just replace it with `None`.
        '''
        encoded_inputs = self._encoded_inputs(input_parameters, input_encoders)
        if self.authorized:
            logger.debug('Authenticated call to api %s', url)
            request = self.oauth.signed_request(url, body=encoded_inputs)
        else:
            logger.debug('Anonymous call to api %s', url)
            request = requests.post(url, data=encoded_inputs)
        return WolframAPIBuilder.build(request, decoder)

class APIUtil(object):
    ''' A utility class that provides some static function to help construct API urls.'''
    @staticmethod
    def user_api_url(username, api_id, server=WolframPublicCloudServer):
        '''Build an API URL from a user name and an API id.
        
        user name is generally the Wolfram Language symbol `$UserName`. 
        API id can be a uuid or a name, in the form of a relative path. 
        e.g: myapi/foo/bar

        The server parameter, by default the Wolfram public Cloud is used
        to provide the base url. See `Server.cloudbase`.
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
