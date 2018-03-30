from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.evaluation.cloud.exceptions import AuthenticationException, XAuthNotConfigured
from wolframclient.evaluation.cloud.oauth import OAuth
from wolframclient.evaluation.cloud.inputoutput import WolframAPIResponse, WolframAPI
import requests

__all__ = ['CloudContext']

class CloudContext(object):
    __slots__ = 'server', 'oauth', 'consumer', 'consumer_secret', 'user', 'password'
    def __init__(self, server, authentication=None):
        self.server = server
        self.oauth = None
        if authentication is not None:
            self.consumer = authentication.consumer_key
            self.consumer_secret = authentication.consumer_secret
        else:
            self.consumer = None
            self.consumer_secret = None

    def anonymous_authentication(self, authentication=None):
        if authentication is not None:
            self.consumer = authentication.consumer_key
            self.consumer_secret = authentication.consumer_secret
        else:
            if self.consumer is None or self.consumer_secret is None:
                raise AuthenticationException('Authentication is missing.')

        self.oauth = OAuth(self.consumer, self.consumer_secret)
        self.oauth.auth()
        
    def user_authentication(self, user, password):
        if not self.server.is_xauth():
            raise XAuthNotConfigured
        self.oauth = OAuth(self.server.xauth_consumer_key, self.server.xauth_consumer_secret)
        self.user = user
        self.password = password
        self.oauth.xauth(user, password)

    def check_auth(self):
        if self.oauth is None:
            raise AuthenticationException('Credentials not set.')

    def execute(self, api, input):
        # TODO encode input if specified by the API
        if not api.public:
            self.check_auth()
            request = self.oauth.signed_request(api.url, body=input)
        else:
            request = requests.post(api.url, data=input)

        return WolframAPIResponse(api, request)

    def public_api(self, url, *input_types, result_type=None):
        return WolframAPI(url, result_type=result_type, public=True)

    def user_api(self, username, api_id, *input_types, result_type=None, public=False):
        '''Build a WolframAPI instance from a user name and an API id.
        
        user name is generally $UserName. API id can be a uuid or a name,
        in the form of a relative path. e.g: myapi/foo/bar
        '''
        builder = URLBuilder(self.server.cloudbase)
        builder.extend('objects', username, api_id)
        url = builder.get()
        return WolframAPI(url, *input_types, result_type=result_type, public=public)

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
