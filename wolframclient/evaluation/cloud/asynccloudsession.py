# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging
import asyncio
import warnings
import ssl
from io import IOBase
from wolframclient.evaluation.cloud.asyncoauth import (
    OAuth1AIOHttpAsyncSession as OAuthAsyncSession,
    XAuthAIOHttpAsyncSession as XAuthAsyncSession)
from wolframclient.evaluation.cloud.server import WolframPublicCloudServer
from wolframclient.evaluation.result import (WolframAPIResponseBuilder,
                                            WolframEvaluationJSONResponseAsync)
from wolframclient.exception import AuthenticationException
from wolframclient.language import wl
from wolframclient.serializers import export
from wolframclient.utils import six
from wolframclient.utils.url import evaluation_api_url, user_api_url
from wolframclient.utils.api import urllib, oauth

from wolframclient.evaluation.base import WolframAsyncEvaluator

from aiohttp import ClientSession, StringPayload, BytesPayload, FormData

logger = logging.getLogger(__name__)

class WolframCloudAsyncSession(WolframAsyncEvaluator):
    
    _stopped = True # avoid error in __del__ if __init__ failed.
    
    def __init__(self, credentials=None, server=WolframPublicCloudServer, loop=None,
                    oauth_session_class=OAuthAsyncSession, 
                    xauth_session_class=XAuthAsyncSession,
                    http_sessionclass=ClientSession,
                    ssl_context_class=ssl.SSLContext):
        super().__init__(loop)
        self.server = server
        self.http_session = None
        self.http_sessionclass = http_sessionclass
        self.credentials = credentials
        self.evaluation_api_url = evaluation_api_url(self.server)
        if self.credentials:
            if self.credentials.is_xauth:
                self.xauth_session_class = xauth_session_class
            else:
                self.oauth_session_class = oauth_session_class
        self.oauth_session = None
        if self.server.certificate is not None:
            self._ssl_context = self.ssl_context_class(self.server.certificate)
        else:
            self._ssl_context = None

    async def start(self):
        self._stopped = False
        if not await self.started():
            if self.http_session is None or self.http_session.closed:
                self.http_session = self.http_sessionclass(
                    headers={'User-Agent': 'WolframClientForPython/1.0'}, 
                    loop=self._loop)
            if not self.anonymous():
                await self.authenticate()

    async def started(self):
        return (self.http_session is not None 
            and (self.anonymous() or self.authorized())
            )

    # @property
    # def authorized(self):
    #     return self.oauth_session is not None and self.oauth_session.started()
    
    # @property
    # def anonymous(self):
    #     return self.credentials is None
        
    def stopped(self):
        return self._stopped

    """ Terminate gracefully stops. """
    async def stop(self):
        await self.terminate()

    async def terminate(self):
        self._stopped = True
        if self.http_session and not self.http_session.closed:
            await self.http_session.close()
        self.http_session = None
        self.oauth_session = None

    def anonymous(self):
        return self.credentials is None
    
    def authorized(self):
        return self.oauth_session is not None and self.oauth_session.authorized()


    async def authenticate(self):
        """Authenticate with the server using the credentials.

        This method supports both oauth and xauth methods. It is not necessary
        to call it, since the session will try to authenticate when the first
        request is issued. """
        logger.info('Authenticating to the server.')
        if self.credentials is None:
            raise AuthenticationException('Missing credentials.')
        if self.credentials.is_xauth:
            self.oauth_session = self.xauth_session_class(
                self.credentials, 
                self.http_session, 
                self.server)
        else:
            self.oauth_session = self.oauth_session_class(
                self.http_session,
                self.server, 
                self.credentials.consumer_key,
                self.credentials.consumer_secret)
        await self.oauth_session.authenticate()

    async def call(self,
             api,
             input_parameters={},
             files={},
             target_format='wl',
             permissions_key=None,
             **kwargv):
        """Call a given API, using the provided input parameters.

        `api` can be a string url or a :class:`tuple` (`username`, `api name`). User name is
        generally the Wolfram Language symbol ``$UserName``. The API name can be a uuid or a
        relative path e.g: *myapi/foo/bar*.

        The input parameters are provided as a dictionary with string keys being the name
        of the parameters associated to their value.

        Files are passed in a dictionary. Value can have multiple forms::

            {'parameter name': file_pointer}

        It is possible to explicitly specify a filename and a content type::

            {'parameter name': ('filename', file_pointer, 'content-type')}

        String can also be passed as files::

            {'parameter name': ('filename', '...string...data...', 'content-type')}

        It's possible to pass a ``PermissionsKey`` to the server along side to the query,
        and get access to a given resource.
        """
        url = user_api_url(self.server, api)
        params = {}
        if permissions_key is not None:
            params['_key'] = permissions_key
        is_multipart = isinstance(files, dict) and len(files) > 0
        encoded_inputs = encode_api_inputs(
            input_parameters,
            files=files,
            target_format=target_format,
            **kwargv)
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Encoded input %s', encoded_inputs)
        # in multipart requests we have to merge input parameters with files.
        # and use the same format for both.
        if is_multipart:
            response = await self._post(url, data=encoded_inputs, params=params)
        else:
            response = await self._post(url, data=encoded_inputs, params=params)

        return WolframAPIResponseBuilder.build(response)

    async def _call_evaluation_api(self, data):
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Sending expression to cloud server for evaluation: %s', data)
        response = await self._post(self.evaluation_api_url, data=data)
        return WolframEvaluationJSONResponseAsync(response)

    async def _post(self, url, headers={}, data=None, params={}):
        """Do a POST request, signing the content only if authentication has been successful."""
        if not self.started:
            await self.start()
        if self.stopped():
            await self.restart()
        headers['User-Agent'] = 'WolframClientForPython/1.0'
        if self.authorized():
            logger.info('Authenticated call to api %s', url)
            return await self.oauth_session.signed_request(url, headers=headers, data=data)
        else:
            logger.info('Anonymous call to api %s', url)
            return await self.http_session.post(url,
                params=params,
                headers=headers,
                data=data,
                ssl=self._ssl_context)

    # def _user_api_url(self, api):
    #     """Build an API URL from a user name and an API id. """
    #     if isinstance(api, tuple) or isinstance(api, list):
    #         if len(api) == 2:
    #             return url_join(self.server.cloudbase, 'objects', api[0],
    #                             api[1])
    #         else:
    #             raise ValueError(
    #                 'Target api specified as a tuple must have two elements: the user name, the API name.'
    #             )
    #     elif isinstance(api, six.string_types):
    #         return api
    #     else:
    #         raise ValueError(
    #             'Invalid API specifications. Expecting string or tuple.')

    # def evaluation_api_url(self):
    #     return url_join(self.server.cloudbase,'evaluations?_responseform=json')

    def _normalize_input(self, expr, **kwargs):
        #TODO: for consistency maybe string and bytearray should be exported as well?
        if isinstance(expr, six.string_types):
            return StringPayload(expr)
        elif isinstance(expr, six.binary_type):
            return BytesPayload(expr)
        else:
            return BytesPayload(export(expr, **kwargs))

    async def evaluate(self, expr, **kwargs):
        """Send `expr` to the cloud for evaluation, return the result.

        `expr` can be a Python object serializable by :func:`~wolframclient.serializers.export`,
        or a the string InputForm of an expression to evaluate.
        """
        response = await self._call_evaluation_api(self._normalize_input(expr, **kwargs))
        return await response.get()

    async def evaluate_wrap(self, expr, **kwargs):
        """ Similar to :func:`~wolframclient.evaluation.cloud.asynccloudsession.WolframCloudAsyncSession.evaluate` but return the result as a :class:`~wolframclient.evaluation.result.WolframEvaluationJSONResponseAsync`.
        """
        return await self._call_evaluation_api(self._normalize_input(expr, **kwargs))


    def __repr__(self):
        return '<{}:base={}, anonymous={}, autorized={}>'.format(self.__class__.__name__, self.server.cloudbase, self.anonymous, self.authorized())


### Some internal utilities focused on cloud data manipulation and 
# formatting for http requests, based on aiohttp objects.

def _encode_inputs_as_wxf(inputs, multipart, **kwargs):
    for name, value in inputs.items():
        form_data.add_field(name + '__wxf', export(value, target_format='wxf', **kwargs))

def _encode_inputs_as_json(form_data, inputs, **kwargs):
    for name, value in inputs.items():
        form_data.add_field(name + '__json', json.dumps(value, **kwargs))

def _encode_inputs_as_wl(form_data, inputs, **kwargs):
    for name, value in inputs.items():
        # avoid double encoding of strings '\"string\"'.
        if isinstance(value, six.string_types):
            form_data.add_field(name, value)
        else:
            form_data.add_field(
                name,
                export(value, target_format='wl', **kwargs))

def update_parameter_list(parameters, name, value, multipart=False):
    """ Update the given :class:`~parameters` with a new inputs using the appropriate form based on `multipart`.
    """
    if multipart:
        parameters[name] = ('tmp_file_%s' % name, value)
    else:
        parameters[name] = value

SUPPORTED_ENCODING_FORMATS = {
    'json': _encode_inputs_as_json,
    'wxf': _encode_inputs_as_wxf,
    'wl': _encode_inputs_as_wl
}


def encode_api_inputs(inputs, files={}, target_format='wl', **kwargs):
    if inputs == {} and files == {}:
        return None
    
    encoder = SUPPORTED_ENCODING_FORMATS.get(target_format, None)
    if encoder is None:
        raise ValueError(
            'Invalid encoding format %s. Choices are: %s' %
            (target_format, ', '.join(SUPPORTED_ENCODING_FORMATS.keys())))
    form_data = FormData()
    for name, file_info in files.items():
        if isinstance(file_info, six.string_types) or isinstance(file_info, six.binary_type) or isinstance(file_info, IOBase):
            form_data.add_field(name, file_info)
        elif isinstance(file_info, tuple) and len(file_info) == 3:
            form_data.add_field(name, file_info[0], filename=file_info[1], content_type=file_info[2])
        else:
            raise ValueError('File parameter must be represented by a string or a triple with the file descriptor, the file name, and the content type.')
    encoder(form_data, inputs, **kwargs)
    return form_data



# Test

async def main():
    from wolframclient.evaluation import SecuredAuthenticationKey
    from wolframclient.tests.configure import secured_authentication_key, user_configuration,server
    print('oauth')
    session = None
    try:
        session = WolframCloudAsyncSession(
            credentials=secured_authentication_key
        )
        await session.start()
        print('start', session.oauth_session._oauth_token, session.oauth_session._oauth_token_secret, await session.started())
        await session.restart()
        print('restart on started', session.oauth_session._oauth_token, session.oauth_session._oauth_token_secret, await session.started())
        await session.stop()
        print('after stop, started?', await session.started())
        await session.restart()
        print('restart after stop', session.oauth_session._oauth_token, session.oauth_session._oauth_token_secret, await session.started())
        await session.restart()
        print('restart on started.', session.oauth_session._oauth_token, session.oauth_session._oauth_token_secret, await session.started())
    except Exception as e:
        raise e
    finally:
        if session:
            await session.stop()
    print('xauth')
    try:
        session = WolframCloudAsyncSession(
            credentials=user_configuration,
            server=server
        )
        await session.start()
        print('start', session.oauth_session._oauth_token, session.oauth_session._oauth_token_secret, await session.started())
        await session.restart()
        print('restart on started', session.oauth_session._oauth_token, session.oauth_session._oauth_token_secret, await session.started())
        await session.stop()
        print('after stop, started?', await session.started())
        await session.restart()
        print('restart after stop', session.oauth_session._oauth_token, session.oauth_session._oauth_token_secret, await session.started())
        await session.restart()
        print('restart on started.', session.oauth_session._oauth_token, session.oauth_session._oauth_token_secret, await session.started())
    except Exception as e:
        raise e
    finally:
        await session.stop()
        await asyncio.sleep(0) #close http session
    
    
    async with WolframCloudAsyncSession(
            credentials=user_configuration,
            server=server,
            loop=asyncio.get_running_loop()
        ) as session:
        await session.start()
        res = await session.evaluate('Range[3]')
        print(res)
        api = ('dorianb', 'api/private/str_image_int')
        with open('/Users/dorianb/Work/Matematica/Workspaces/WolframClientForPython/wolframclient/tests/data/32x2.png', 'rb') as fp:
            response = await session.call(api, input_parameters={'str':'abc', 'int' : 10}, files={'image': fp})
            import json
            res = json.loads(await response.get())
        print(res)
    
if __name__ == '__main__':
    from wolframclient.logger.utils import setup_logging_to_file
    setup_logging_to_file('/tmp/python.log', level=logging.DEBUG)
    print('start')
    asyncio.run(main())
    print('stop')
