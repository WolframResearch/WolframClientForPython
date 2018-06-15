# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.evaluation.cloud.inputoutput import WolframAPIResponseBuilder, WolframEvaluationResponse
from wolframclient.evaluation.cloud.oauth import OAuthSession
from wolframclient.evaluation.cloud.server import WolframPublicCloudServer
from wolframclient.exception import AuthenticationException
from wolframclient.language.expression import wl
from wolframclient.serializers import export
from wolframclient.utils import six
if six.PY3:
    from concurrent.futures import ThreadPoolExecutor
from wolframclient.utils.api import json, requests

import logging

logger = logging.getLogger(__name__)

__all__ = ['WolframCloudSession']


class WolframCloudSession(object):
    ''' Represents a session to a given cloud enabling simple API call.

    This is the central class of the cloud evaluation package. It is
    initialized with a server instance representing a given cloud. The
    `default` static method can be used to initialize a session to the Wolfram
    public cloud.

    Most of the time it is necessary to authenticate with the server before issuing
    requests. `WolframCloudSession` supports two forms of authentication:
    - 2-legged oauth using a secured authentication key.
    - xauth using the user ID and password.

    Calling an API is done throught the method `call` which will return an instance of
    a `WolframAPIResponse`. It is strongly advised to re-use a session to make multiple
    calls.
    '''
    __slots__ = 'server', 'oauth', 'consumer', 'consumer_secret', 'user', 'password', 'is_xauth', 'evaluation_api_url', 'authentication', 'thread_pool_exec'

    def __init__(self, authentication=None, server=WolframPublicCloudServer):
        self.server = server
        # user provided information to authenticate.
        self.authentication = authentication
        self.evaluation_api_url = self._evaluation_api_url()
        self.oauth = None
        self.consumer = None
        self.consumer_secret = None
        self.user = None
        self.password = None
        self.is_xauth = None
        self.thread_pool_exec = None

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.terminate()

    def terminate(self):
        if self.thread_pool_exec is not None:
            self.thread_pool_exec.shutdown(wait=True)

    def authenticate(self):
        '''Authenticate with the server using the credentials.

        This method supports both oauth and xauth methods. It is not necessary
        to call it, since the session will try to authenticate when the first
        request is issued. '''
        logger.info('Authenticating to the server.')
        if self.authentication is None:
            raise AuthenticationException('Missing authentication.')
        if self.authentication.is_xauth:
            self.user_authentication()
        else:
            self.sak_authentication()

    def sak_authentication(self):
        self.consumer = self.authentication.consumer_key
        self.consumer_secret = self.authentication.consumer_secret
        self.is_xauth = False
        self.oauth = OAuthSession(
            self.server, self.consumer, self.consumer_secret)
        self.oauth.auth()

    def user_authentication(self):
        if not self.server.is_xauth():
            raise AuthenticationException('XAuth is not configured. Missing consumer key and/or secret.')
        self.user = self.authentication.user
        self.password = self.authentication.password
        self.is_xauth = True
        self.oauth = OAuthSession(self.server, self.server.xauth_consumer_key,
                                  self.server.xauth_consumer_secret)
        self.oauth.xauth(self.authentication.user,
                         self.authentication.password)

    @property
    def authorized(self):
        ''' Returns a reasonnably accurate state of the authentication status. '''
        if self.authentication is not None and self.is_xauth is None:
            self.authenticate()
        if self.is_xauth is None or self.oauth is None:
            return False
        else:
            return (bool(self.oauth._client.client_secret) and
                    bool(self.oauth._client.resource_owner_key) and
                    bool(self.oauth._client.resource_owner_secret))

    def _post(self, url, headers={}, body={}, params={}):
        ''' Do a POST request, signing the content only if authentication has
        been successful. '''
        headers['User-Agent'] = 'WolframClientForPython/1.0'
        if self.authorized:
            logger.info('Authenticated call to api %s', url)
            return self.oauth.signed_request(url, headers=headers, body=body)
        else:
            logger.info('Anonymous call to api %s', url)
            return requests.post(
                url, params=params, headers=headers, data=body, verify=self.server.verify)

    def call(self, api, input_parameters={}, target_format='wl', permissions_key=None, **kwargv):
        ''' Call a given API, using the provided input parameters.

        `api` can be a string url or a tuple (`username`, `api name`). User name is
        generally the Wolfram Language symbol `$UserName`. API id can be a uuid or a
        name, in the form of a relative path. e.g: myapi/foo/bar

        The input parameters are provider as a dictionary with string keys being the name
        of the parameters associated to their value. The input encoder(s) can be specified
        as a callable in which case it is applied to all inputs, or as a dictionary, with keys
        being the parameter names, for a finer control of the encoding. Finally it is possible
        to specify a decoder, which is applied when the request was successful, to the raw binary
        response of the API.

        It's possible to specify a PermissionsKey passed to the server along side the query to
        get access to a given resource.

        Note: By default a decoder is specified and ensure the response is of type string. To
        get raw bytes just replace it with `None`.
        '''
        url = self._user_api_url(api)
        encoded_inputs = encode_api_inputs(
            input_parameters, target_format=target_format, **kwargv)
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Encoded input %s', encoded_inputs)

        params = {'_key': permissions_key} if permissions_key is not None else {}
        response = self._post(url, body=encoded_inputs, params=params)

        return WolframAPIResponseBuilder.build(response)

    def _user_api_url(self, api):
        '''Build an API URL from a user name and an API id. '''
        if isinstance(api, tuple) or isinstance(api, list):
            if len(api) == 2:
                return url_join(self.server.cloudbase, 'objects', api[0], api[1])
            else:
                raise ValueError(
                    'Target api specified as a tuple must have two elements: the user name, the API name.')
        elif isinstance(api, six.string_types):
            return api
        else:
            raise ValueError(
                'Invalid API description. Expecting string or tuple.')

    def _evaluation_api_url(self):
        return url_join(self.server.cloudbase, 'evaluations?_responseform=json')
        # TODO tmp otherwise every call fails
        # return url_join(self.server.cloudbase, 'evaluations')

    def _call_evaluation_api(self, data):
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug(
                'Sending expression to cloud server for evaluation: %s', data)
        if not isinstance(data, six.string_types) and not isinstance(data, six.binary_type):
            raise ValueError('Expecting string input, unicode or binary.')
        response = self._post(
            self.evaluation_api_url,
            body=data)
        return WolframEvaluationResponse(response)

    def evaluate_string(self, expr):
        ''' Send the string InputForm of an `expr` to the cloud for evaluation. '''
        return self._call_evaluation_api(expr)

    def evaluate(self, expr):
        ''' Send `expr` to the cloud for evaluation.

        `expr` must be a Python object serializable by `wolframclient.serializers.export`
        '''
        return self._call_evaluation_api(export(expr))
    
    if six.PY3:
        def _thread_pool_exec(self):
            if self.thread_pool_exec is None:
                self.thread_pool_exec = ThreadPoolExecutor()
            return self.thread_pool_exec

        def call_async(self, api, input_parameters={}, target_format='wl', permissions_key=None, **kwargv):
            ''' Call a given API asynchronously. Returns a
            :class:`concurrent.futures.Future` object.

            See :func:`WolframCloudSession.call`.
            '''
            return self._thread_pool_exec().submit(
                self.call, api, input_parameters, target_format, permissions_key, **kwargv)

        def _evaluate_async(self, data):
            return self._thread_pool_exec().submit(self._call_evaluation_api, data)

        def evaluate_async(self, expr):
            ''' Send `expr` to the cloud for asynchronous evaluation. Returns a
            :class:`concurrent.futures.Future` object.

            `expr` must be a Python object serializable by `wolframclient.serializers.export`
            '''
            return self._evaluate_async(export(expr))

        def evaluate_string_async(self, expr):
            ''' Send the string InputForm of an `expr` to the cloud for asynchronous evaluation.
            Returns a :class:`concurrent.futures.Future` object.
            '''
            return self._evaluate_async(expr)

        def cloud_function(self, func, asynchronous=False):
            return CloudFunction(self, func, asynchronous=asynchronous)
    # Not python3, no asynchronous evaluation.
    else: 
        def cloud_function(self, func):
            return CloudFunction(self, func)

    def __repr__(self):
        return '<{}:base={}, authorized={}>'.format(self.__class__.__name__, self.server.cloudbase, self.authorized)


class CloudFunction(object):
    if six.PY3:
        def __init__(self, session, func, asynchronous=False):
            self.session = session
            self.func = func
            if not six.PY3 and asynchronous:
                raise Warning('Asynchronous')
            if asynchronous:
                self.evaluation_func = WolframCloudSession.evaluate
            else:
                self.evaluation_func = WolframCloudSession.evaluate_async
    else:
        def __init__(self, session, func):
            self.session = session
            self.func = func
            self.evaluation_func = WolframCloudSession.evaluate_async

    def __call__(self, *args):
        return self.evaluation_func(wl.Construct(self.func, *args))


def _encode_inputs_as_wxf(inputs, **kwargs):
    encoded_inputs = {}
    for name, value in inputs.items():
        name = name + '__wxf'
        encoded_inputs[name] = export(value, target_format='wxf', **kwargs)
    return encoded_inputs


def _encode_inputs_as_json(inputs, **kwargs):
    encoded_inputs = {}
    for name, value in inputs.items():
        name = name + '__json'
        encoded_inputs[name] = json.dumps(value, **kwargs)
    return encoded_inputs


def _encode_inputs_as_wl(inputs, **kwargs):
    encoded_inputs = {}
    for name, value in inputs.items():
        # avoid double encoding of strings '\"string\"'.
        if isinstance(value, six.string_types):
            encoded_inputs[name] = value
        else:
            encoded_inputs[name] = export(value, target_format='wl', **kwargs)
    return encoded_inputs

SUPPORTED_ENCODING_FORMATS = {
    'json': _encode_inputs_as_json,
    'wxf':  _encode_inputs_as_wxf,
    'wl':   _encode_inputs_as_wl
}

def encode_api_inputs(inputs, target_format='wl', **kwargs):
    if len(inputs) == 0:
        return {}

    encoder = SUPPORTED_ENCODING_FORMATS.get(target_format, None)
    if encoder is None:
        raise ValueError('Invalid encoding format %s. Choices are: %s' % (
            target_format, ', '.join(SUPPORTED_ENCODING_FORMATS.keys())))

    return encoder(inputs, **kwargs)

def url_join(*fragments):
    ''' Join fragments of a URL, dealing with slashes.'''
    if len(fragments) == 0:
        return ''
    buff = []
    for fragment in fragments:
        stripped = fragment.strip('/')
        if len(stripped) > 0:
            buff.append(stripped)
            buff.append('/')

    last = fragments[-1]
    # add a trailing '/' if present.
    if len(last) > 0 and last[-1] != '/':
        buff.pop()
    return ''.join(buff)
