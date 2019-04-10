# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging

from wolframclient.evaluation.base import WolframAsyncEvaluator
from wolframclient.evaluation.cloud.asyncoauth import \
    OAuth1AIOHttpAsyncSession as OAuthAsyncSession
from wolframclient.evaluation.cloud.asyncoauth import \
    XAuthAIOHttpAsyncSession as XAuthAsyncSession
from wolframclient.evaluation.cloud.base import WolframAPICallBase
from wolframclient.evaluation.cloud.server import WOLFRAM_PUBLIC_CLOUD_SERVER
from wolframclient.evaluation.result import (WolframAPIResponseBuilder,
                                             WolframEvaluationWXFResponseAsync)
from wolframclient.exception import AuthenticationException
from wolframclient.serializers import export
from wolframclient.utils import six
from wolframclient.utils.api import aiohttp, ssl
from wolframclient.utils.url import evaluation_api_url, user_api_url

logger = logging.getLogger(__name__)

__all__ = ['WolframCloudAsyncSession', 'WolframAPICallAsync']


class WolframCloudAsyncSession(WolframAsyncEvaluator):
    """ Interact with a Wolfram Cloud asynchronously using coroutines.

    Asynchronous cloud operations are provided through coroutines using modules :mod:`asyncio` and
    `aiohttp <https://pypi.org/project/aiohttp/>`_.

    Instances of this class can be managed with an asynchronous context manager::

        async with WolframCloudAsyncSession() as session:
            await session.call(...)

    An event loop can be explicitly passed using the named parameter `loop`; otherwise, the one
    returned by :func:`~asyncio.get_event_loop` is used.
    The initialization options of the class :class:`~wolframclient.evaluation.WolframCloudSession` are also supported by
    this class.
    """
    def __init__(self,
                 credentials=None,
                 server=None,
                 loop=None,
                 inputform_string_evaluation=True,
                 oauth_session_class=None,
                 xauth_session_class=None,
                 http_sessionclass=None,
                 ssl_context_class=None):
        super().__init__(
            loop, inputform_string_evaluation=inputform_string_evaluation)
        self.server = server or WOLFRAM_PUBLIC_CLOUD_SERVER
        self.http_session = None
        self.http_sessionclass = http_sessionclass or aiohttp.ClientSession
        self.credentials = credentials
        self.evaluation_api_url = evaluation_api_url(self.server)
        self.xauth_session_class = xauth_session_class or XAuthAsyncSession
        self.oauth_session_class = oauth_session_class or OAuthAsyncSession
        self.ssl_context_class = ssl_context_class or ssl.SSLContext
        self.oauth_session = None
        if self.server.certificate is not None:
            self._ssl_context = self.ssl_context_class()
            self._ssl_context.load_verify_locations(self.server.certificate)
            # self._ssl_context = ssl.create_default_context(cafile=self.server.certificate)
        else:
            self._ssl_context = None

    def duplicate(self):
        return self.__class__(
            credentials=self.credentials,
            server=self.server,
            loop=self._loop,
            inputform_string_evaluation=self.inputform_string_evaluation,
            oauth_session_class=self.oauth_session_class,
            xauth_session_class=self.xauth_session_class,
            http_sessionclass=self.http_sessionclass,
            ssl_context_class=self.ssl_context_class)

    async def start(self):
        self.stopped = False
        try:
            if not self.started:
                if self.http_session is None or self.http_session.closed:
                    self.http_session = self.http_sessionclass(
                        headers={'User-Agent': 'WolframClientForPython/1.0'},
                        loop=self._loop)
                if not self.anonymous():
                    await self._authenticate()
        except Exception as e:
            try:
                await self.terminate()
            finally:
                raise e

    @property
    def started(self):
        return self.http_session is not None and (self.anonymous()
                                                  or self.authorized())

    """ Terminate gracefully stops. """

    async def stop(self):
        await self.terminate()

    async def terminate(self):
        self.stopped = True
        if self.http_session:
            await self.http_session.close()
        self.http_session = None
        self.oauth_session = None

    def anonymous(self):
        return self.credentials is None

    def authorized(self):
        return self.oauth_session is not None and self.oauth_session.authorized(
        )

    async def _authenticate(self):
        """Authenticate with the server using the credentials.

        This method supports both oauth and xauth methods. It is not necessary
        to call it, since the session will try to authenticate when the first
        request is issued. """
        logger.info('Authenticating to the server.')
        if self.credentials is None:
            raise AuthenticationException('Missing credentials.')
        if self.credentials.is_xauth:
            self.oauth_session = self.xauth_session_class(
                self.credentials, self.http_session, self.server)
        else:
            self.oauth_session = self.oauth_session_class(
                self.http_session, self.server, self.credentials.consumer_key,
                self.credentials.consumer_secret)
        await self.oauth_session.authenticate()

    async def call(self,
                   api,
                   input_parameters={},
                   files={},
                   target_format='wl',
                   permissions_key=None,
                   **kwargv):
        """Call a given API using the provided input parameters.

        `api` can be a string url or a :class:`tuple` (`username`, `api name`). The username is generally the Wolfram
        Language symbol ``$UserName``. The API name can be a UUID or a relative path, e.g. *myapi/foo/bar*.

        The input parameters are provided as a dictionary with string keys being the name
        of the parameters associated to their value.

        Files are passed in a dictionary. Values can have multiple forms::

            {'parameter name': file_pointer}

        It is possible to explicitly specify a filename and a content type::

            {'parameter name': ('filename', file_pointer, 'content-type')}

        Bytes can also be passed as files::

            {'parameter name': ('filename', b'...binary...data...', 'content-type')}

        It is possible to pass a ``PermissionsKey`` to the server alongside the query and get access to a given
        resource.
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
            response = await self._post(
                url, data=encoded_inputs, params=params)
        else:
            response = await self._post(
                url, data=encoded_inputs, params=params)

        return WolframAPIResponseBuilder.build(response)

    async def _post(self, url, headers={}, data=None, params={}):
        """Do a POST request, signing the content only if authentication has been successful."""
        if not self.started:
            await self.start()
        if self.stopped:
            await self.restart()
        headers['User-Agent'] = 'WolframClientForPython/1.0'
        if self.authorized():
            logger.info('Authenticated call to api %s', url)
            return await self.oauth_session.signed_request(
                url, headers=headers, data=data)
        else:
            logger.info('Anonymous call to api %s', url)
            return await self.http_session.post(
                url,
                params=params,
                headers=headers,
                data=data,
                ssl=self._ssl_context)

    async def _call_evaluation_api(self, expr, **kwargs):
        data = aiohttp.BytesPayload(export(expr, target_format='wl', **kwargs))
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug(
                'Sending expression to cloud server for evaluation: %s', data)
        response = await self._post(self.evaluation_api_url, data=data)
        return WolframEvaluationWXFResponseAsync(response)

    async def evaluate(self, expr, **kwargs):
        """Send `expr` to the cloud for evaluation and return the result.

        `expr` can be a Python object serializable by :func:`~wolframclient.serializers.export` or the string
        :wl:`InputForm` of an expression to evaluate.
        """
        response = await self._call_evaluation_api(
            self.normalize_input(expr), **kwargs)
        return await response.get()

    async def evaluate_wrap(self, expr, **kwargs):
        """ Similar to :func:`~wolframclient.evaluation.cloud.asynccloudsession.WolframCloudAsyncSession.evaluate` but
        return the result as a :class:`~wolframclient.evaluation.result.WolframEvaluationJSONResponseAsync`.
        """
        return await self._call_evaluation_api(
            self.normalize_input(expr), **kwargs)

    def wolfram_api_call(self, api, **kwargs):
        """ Build an helper class instance to call a given API. """
        return WolframAPICallAsync(self, api, **kwargs)

    def __repr__(self):
        return '<{}:base={}, anonymous={}, autorized={}>'.format(
            self.__class__.__name__, self.server.cloudbase, self.anonymous(),
            self.authorized())


class WolframAPICallAsync(WolframAPICallBase):
    """Perform an API call using an asynchronous cloud session. """

    async def perform(self, **kwargs):
        """Make the API call and return the result."""
        return await self.target.call(
            self.api,
            input_parameters=self.parameters,
            files=self.files,
            permissions_key=self.permission_key,
            **kwargs)


### Some internal utilities focused on cloud data manipulation and
# formatting for http requests, based on aiohttp objects.


def _encode_inputs_as_wxf(inputs, multipart, **kwargs):
    for name, value in inputs.items():
        form_data.add_field(name + '__wxf',
                            export(value, target_format='wxf', **kwargs))


def _encode_inputs_as_json(form_data, inputs, **kwargs):
    for name, value in inputs.items():
        form_data.add_field(name + '__json', json.dumps(value, **kwargs))


def _encode_inputs_as_wl(form_data, inputs, **kwargs):
    for name, value in inputs.items():
        # avoid double encoding of strings '\"string\"'.
        if isinstance(value, six.string_types):
            form_data.add_field(name, value)
        else:
            form_data.add_field(name,
                                export(value, target_format='wl', **kwargs))


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
    form_data = aiohttp.FormData()
    # files are specified by file pointer or bytes, or a tuple.
    for name, file_info in files.items():
        # tuple must contain: the filename, the data as bytes, the content type.
        if isinstance(file_info, tuple) and len(file_info) == 3:
            form_data.add_field(
                name,
                file_info[1],
                filename=file_info[0],
                content_type=file_info[2])
        # otherwise it must be the filename. Delegate input validation to FormData:
        else:
            form_data.add_field(name, file_info)
    encoder(form_data, inputs, **kwargs)
    return form_data
