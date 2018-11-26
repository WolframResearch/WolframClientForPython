# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging

from wolframclient.evaluation.base import WolframEvaluator
from wolframclient.evaluation.cloud.base import WolframAPICallBase
from wolframclient.evaluation.cloud.oauth import \
    OAuth1RequestsSyncSession as OAuthSession
from wolframclient.evaluation.cloud.oauth import \
    XAuthRequestsSyncSession as XAuthSession
from wolframclient.evaluation.cloud.server import WolframPublicCloudServer
from wolframclient.evaluation.result import (WolframAPIResponseBuilder,
                                             WolframEvaluationJSONResponse)
from wolframclient.exception import AuthenticationException
from wolframclient.serializers import export
from wolframclient.utils import six
from wolframclient.utils.api import futures, json, requests
from wolframclient.utils.url import evaluation_api_url, user_api_url

logger = logging.getLogger(__name__)

__all__ = [
    'WolframCloudSession', 'WolframCloudSessionFuture', 'WolframAPICall'
]


class WolframCloudSession(WolframEvaluator):
    """Represent a session to a given cloud enabling simple API call.

    This is the central class of the cloud evaluation package. It is initialized with a server instance
    representing a given cloud. By default a session targets the Wolfram public cloud.

    Most of the time it is necessary to authenticate with the server before issuing requests. Session
    supports two forms of authentication:

    * 2-legged oauth using a secured authentication key.
    * xauth using the user ID and password.

    Calling an API is done through the method :func:`~wolframclient.evaluation.cloud.cloudsession.WolframCloudSession.call`
    which will return an instance of :class:`~wolframclient.evaluation.result.WolframAPIResponse`.
    It is strongly advised to re-use a session to make multiple calls to mitigate the cost of initialization.
    """

    def __init__(self,
                 credentials=None,
                 server=WolframPublicCloudServer,
                 inputform_string_evaluation=True,
                 oauth_session_class=OAuthSession,
                 xauth_session_class=XAuthSession,
                 http_sessionclass=requests.Session):
        super().__init__(
            inputform_string_evaluation=inputform_string_evaluation)
        self.server = server
        self.evaluation_api_url = evaluation_api_url(self.server)
        self.http_sessionclass = http_sessionclass
        self.http_session = None
        self.credentials = credentials
        self.evaluation_api_url = evaluation_api_url(self.server)
        if self.credentials:
            if self.credentials.is_xauth:
                self.xauth_session_class = xauth_session_class
            else:
                self.oauth_session_class = oauth_session_class
        self.oauth_session = None
        self.verify = self.server.certificate

    @property
    def started(self):
        return self.http_session is not None and (self.anonymous()
                                                  or self.authorized())

    def start(self):
        self.stopped = False
        if not self.started:
            if self.http_session is None:
                self.http_session = self.http_sessionclass()
                self.http_session.headers = {
                    'User-Agent': 'WolframClientForPython/1.0'
                }
            if not self.anonymous():
                self._authenticate()

    def stop(self):
        self.terminate()

    def terminate(self):
        self.stopped = True
        if self.http_session:
            self.http_session.close()
            self.http_session = None
        self.oauth_session = None

    def anonymous(self):
        return self.credentials is None

    def authorized(self):
        return self.oauth_session is not None and self.oauth_session.authorized(
        )

    def _authenticate(self):
        """Authenticate with the server using the credentials.

        This method supports both oauth and xauth methods. It is not necessary
        to call it, since the session will try to authenticate when the first
        request is issued. """
        logger.info('Authenticating to the server.')
        if self.credentials is None:
            raise AuthenticationException('Missing credentials.')
        if self.credentials.is_xauth:
            self.oauth_session = self.xauth_session_class(
                self.credentials, self.http_session, self.server,
                self.server.xauth_consumer_key,
                self.server.xauth_consumer_secret)
        else:
            self.oauth_session = self.oauth_session_class(
                self.http_session, self.server, self.credentials.consumer_key,
                self.credentials.consumer_secret)
        self.oauth_session.authenticate()

    def _post(self, url, headers={}, body={}, files={}, params={}):
        """Do a POST request, signing the content only if authentication has been successful."""
        self._ensure_started()
        headers['User-Agent'] = 'WolframClientForPython/1.0'
        if self.authorized():
            logger.info('Authenticated call to api %s', url)
            return self.oauth_session.signed_request(
                url, headers=headers, body=body, files=files)
        else:
            logger.info('Anonymous call to api %s', url)
            return self.http_session.post(
                url,
                params=params,
                headers=headers,
                data=body,
                files=files,
                verify=self.verify)

    def _ensure_started(self):
        if not self.started:
            self.start()
        if self.stopped:
            self.restart()

    def call(self,
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
        params = {
            '_key': permissions_key
        } if permissions_key is not None else {}
        is_multipart = isinstance(files, dict) and len(files) > 0
        encoded_inputs = encode_api_inputs(
            input_parameters,
            target_format=target_format,
            multipart=is_multipart,
            **kwargv)
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Encoded input %s', encoded_inputs)
        # in multipart requests we have to merge input parameters with files.
        # and use the same format for both.
        if is_multipart:
            encoded_inputs.update(files)
            response = self._post(url, files=encoded_inputs, params=params)
        else:
            response = self._post(url, body=encoded_inputs, params=params)

        return WolframAPIResponseBuilder.build(response)

    def _call_evaluation_api(self, expr, **kwargs):
        data = export(expr, target_format='wl', **kwargs)
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug(
                'Sending expression to cloud server for evaluation: %s', data)
        response = self._post(self.evaluation_api_url, body=data)
        return WolframEvaluationJSONResponse(response)

    def evaluate(self, expr, **kwargs):
        """Send `expr` to the cloud for evaluation, return the result.

        `expr` can be a Python object serializable by :func:`~wolframclient.serializers.export`,
        or a the string InputForm of an expression to evaluate.
        """
        return self._call_evaluation_api(self.normalize_input(expr),
                                         **kwargs).get()

    def evaluate_wrap(self, expr, **kwargs):
        """ Similar to :func:`~wolframclient.evaluation.cloud.cloudsession.WolframCloudSession.evaluate` but return the result as a :class:`~wolframclient.evaluation.result.WolframEvaluationJSONResponse`.
        """
        return self._call_evaluation_api(self.normalize_input(expr), **kwargs)

    def wolfram_api_call(self, api, **kwargs):
        """ Build an helper class instance to call a given API. """
        return WolframAPICall(self, api, **kwargs)

    def __repr__(self):
        return '<{}:base={}, authorized={}>'.format(self.__class__.__name__,
                                                    self.server.cloudbase,
                                                    self.authorized())


class WolframCloudSessionFuture(WolframCloudSession):
    """ Am asynchronous Wolfram cloud session wrapping API calls and cloud evaluations 
    in future objects.

    `max_workers` can be specified and is passed to the ThreadPoolExecutor.
    `kwargs` are parameters as expect by `~wolframclient.evaluation.WolframCloudSession`
    """

    def __init__(self,
                 credentials=None,
                 server=WolframPublicCloudServer,
                 max_workers=None,
                 inputform_string_evaluation=True,
                 oauth_session_class=OAuthSession,
                 xauth_session_class=XAuthSession,
                 http_sessionclass=requests.Session):
        super().__init__(
            credentials=credentials,
            server=server,
            inputform_string_evaluation=inputform_string_evaluation,
            oauth_session_class=oauth_session_class,
            xauth_session_class=xauth_session_class,
            http_sessionclass=http_sessionclass)
        self._pool = None
        self._max_workers = max_workers

    @property
    def started(self):
        return self._pool is not None and super().started

    def start(self):
        super().start()
        if self._pool is None:
            self._pool = futures.ThreadPoolExecutor(
                max_workers=self._max_workers)

    def stop(self):
        if self._pool is not None:
            self._pool.shutdown(wait=True)
            self._pool = None
        super().stop()

    def terminate(self):
        if self._pool is not None:
            self._pool.shutdown(wait=False)
            self._pool = None
        super().terminate()

    def call(self,
             api,
             input_parameters={},
             target_format='wl',
             permissions_key=None,
             **kwargv):
        """Call a given API asynchronously. Returns a :class:`concurrent.futures.Future` object.

        See :func:`WolframCloudSession.call` for more details about input parameters.
        """
        self._ensure_started()
        return self._pool.submit(
            super().call,
            api,
            input_parameters=input_parameters,
            target_format=target_format,
            permissions_key=permissions_key,
            **kwargv)

    def evaluate(self, expr):
        """Send `expr` to the cloud for asynchronous evaluation.

        Returns a :class:`concurrent.futures.Future` object.

        `expr` can be a Python object serializable by :func:`~wolframclient.serializers.export`,
        or a the string InputForm of an expression to evaluate.
        """
        self._ensure_started()
        return self._pool.submit(super().evaluate, expr)


class WolframAPICall(WolframAPICallBase):
    """Helper class to perform an API call using a cloud session. """

    def perform(self, **kwargs):
        """Make the API call, return the result."""
        return self.target.call(
            self.api,
            input_parameters=self.parameters,
            files=self.files,
            permissions_key=self.permission_key,
            **kwargs)


def _encode_inputs_as_wxf(inputs, multipart, **kwargs):
    encoded_inputs = {}
    for name, value in inputs.items():
        wxf_name = name + '__wxf'
        wxf_value = export(value, target_format='wxf', **kwargs)
        update_parameter_list(encoded_inputs, wxf_name, wxf_value, multipart)
    return encoded_inputs


def _encode_inputs_as_json(inputs, multipart, **kwargs):
    encoded_inputs = {}
    for name, value in inputs.items():
        name = name + '__json'
        json_value = json.dumps(value, **kwargs)
        update_parameter_list(encoded_inputs, name, json_value, multipart)
    return encoded_inputs


def _encode_inputs_as_wl(inputs, multipart, **kwargs):
    encoded_inputs = {}
    for name, value in inputs.items():
        # avoid double encoding of strings '\"string\"'.
        if isinstance(value, six.string_types):
            update_parameter_list(encoded_inputs, name, value, multipart)
        else:
            exported_value = export(value, target_format='wl', **kwargs)
            update_parameter_list(encoded_inputs, name, exported_value,
                                  multipart)
    return encoded_inputs


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


def encode_api_inputs(inputs, target_format='wl', multipart=False, **kwargs):
    if len(inputs) == 0:
        return {}

    encoder = SUPPORTED_ENCODING_FORMATS.get(target_format, None)
    if encoder is None:
        raise ValueError(
            'Invalid encoding format %s. Choices are: %s' %
            (target_format, ', '.join(SUPPORTED_ENCODING_FORMATS.keys())))

    return encoder(inputs, multipart, **kwargs)
