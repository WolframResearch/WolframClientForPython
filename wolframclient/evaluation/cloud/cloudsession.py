# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging

from wolframclient.evaluation.cloud.oauth import (
    OAuth1RequestsSyncSession as OAuthSession,  XAuthRequestsSyncSession as XAuthSession)
from wolframclient.evaluation.cloud.server import WolframPublicCloudServer
from wolframclient.evaluation.base import WolframEvaluator
from wolframclient.evaluation.result import (WolframAPIResponseBuilder,
                                             WolframEvaluationJSONResponse)
from wolframclient.exception import AuthenticationException
from wolframclient.language import wl
from wolframclient.serializers import export
from wolframclient.utils.url import url_join, evaluation_api_url, user_api_url
from wolframclient.utils import six
from wolframclient.utils.api import futures, json, requests

logger = logging.getLogger(__name__)

__all__ = ['WolframCloudSession', 'WolframCloudSessionAsync', 'WolframAPICall']


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

    __slots__ = 'server', 'oauth', 'consumer', 'consumer_secret', 'user', 'password', 'evaluation_api_url', 'credentials'
    '_stopped'

    def __init__(self, credentials=None, server=WolframPublicCloudServer,
                    oauth_session_class=OAuthSession, 
                    xauth_session_class=XAuthSession):
        self.server = server
        self.evaluation_api_url = evaluation_api_url(self.server)
        self._stopped = False
        self.credentials = credentials
        self.evaluation_api_url = evaluation_api_url(self.server)
        if self.credentials:
            if self.credentials.is_xauth:
                self.xauth_session_class = xauth_session_class
            else:
                self.oauth_session_class = oauth_session_class
        self.oauth_session = None
        self.verify = self.server.certificate

    def started(self):
        return (self.anonymous() or self.authorized())

    def start(self):
        self._stopped = False
        if not self.started():
            self.authenticate()

    def stopped(self):
        return self._stopped    

    def stop(self):
        self.terminate()
    
    def terminate(self):
        self._stopped = True
        self.oauth_session = None

    def anonymous(self):
        return self.credentials is None
    
    def authorized(self):
        return self.oauth_session is not None and self.oauth_session.authorized()

    def authenticate(self):
        """Authenticate with the server using the credentials.

        This method supports both oauth and xauth methods. It is not necessary
        to call it, since the session will try to authenticate when the first
        request is issued. """
        logger.info('Authenticating to the server.')
        if self.credentials is None:
            raise AuthenticationException('Missing credentials.')
        if self.credentials.is_xauth:
            self.oauth_session = self.xauth_session_class(
                self.credentials, self.server, self.server.xauth_consumer_key,
                self.server.xauth_consumer_secret
            )
        else:
            self.oauth_session = self.oauth_session_class(self.server, self.credentials.consumer_key, self.credentials.consumer_secret)
        self.oauth_session.authenticate()

    # def sak_authentication(self):
    #     self.consumer = self.credentials.consumer_key
    #     self.consumer_secret = self.credentials.consumer_secret
    #     self.is_xauth = False
    #     self.oauth = OAuthSession(self.server, self.consumer,
    #                               self.consumer_secret)
    #     self.oauth.auth()

    # def user_authentication(self):
    #     if not self.server.is_xauth():
    #         raise AuthenticationException(
    #             'XAuth is not configured. Missing consumer key and/or secret.')
    #     self.user = self.credentials.user
    #     self.password = self.credentials.password
    #     self.is_xauth = True
    #     self.oauth = OAuthSession(self.server, self.server.xauth_consumer_key,
    #                               self.server.xauth_consumer_secret)
    #     self.oauth.xauth(self.credentials.user,
    #                      self.credentials.password)

    # def authorized(self):
    #     """Return a reasonnably accurate state of the authentication status."""
    #     # No credentials provided. This session is not authorized (public access).
    #     if self.credentials is None:
    #         return False
    #     # Authentication credentials were provided, but self.is_xauth remains None
    #     # until the first attempt. First need to authenticate first.
    #     if self.is_xauth is None:
    #         self.authenticate()
    #     # if is_xauth was set, ensure the session is authorized.
    #     if self.oauth is not None and self.oauth.started():
    #         return True
    #     # is_xauth was set but the process authentication already failed once. Retry.
    #     else:
    #         logger.warn(
    #             'Not authenticated. Retrying to authenticate with the server.')
    #         self.authenticate()
    #         return self.oauth is not None and self.oauth.started()

    def _post(self, url, headers={}, body={}, files={}, params={}):
        """Do a POST request, signing the content only if authentication has been successful."""
        headers['User-Agent'] = 'WolframClientForPython/1.0'
        if not self.started():
            self.start()
        if self.stopped():
            self.restart()
        if self.authorized():
            logger.info('Authenticated call to api %s', url)
            return self.oauth_session.signed_request(
                url, headers=headers, body=body, files=files)
        else:
            logger.info('Anonymous call to api %s', url)
            return requests.post(
                url,
                params=params,
                headers=headers,
                data=body,
                files=files,
                verify=self.verify)

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
    #             'Invalid API description. Expecting string or tuple.')

    # def evaluation_api_url(self):
    #     return url_join(self.server.cloudbase,
    #                     'evaluations?_responseform=json')

    def _call_evaluation_api(self, data):
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug(
                'Sending expression to cloud server for evaluation: %s', data)
        if not isinstance(data, six.string_types) and not isinstance(
                data, six.binary_type):
            raise ValueError('Expecting string input, unicode or binary.')
        response = self._post(self.evaluation_api_url, body=data)
        return WolframEvaluationJSONResponse(response)

    def _normalize_input(self, expr, **kwargs):
        if isinstance(expr, six.string_types) or isinstance(
                expr, six.binary_type):
            return expr
        else:
            return export(expr, **kwargs)

    def evaluate(self, expr, **kwargs):
        """Send `expr` to the cloud for evaluation, return the result.

        `expr` can be a Python object serializable by :func:`~wolframclient.serializers.export`,
        or a the string InputForm of an expression to evaluate.
        """
        return self._call_evaluation_api(
            self._normalize_input(expr, **kwargs)).get()

    def evaluate_wrap(self, expr, **kwargs):
        """ Similar to :func:`~wolframclient.evaluation.cloud.cloudsession.WolframCloudSession.evaluate` but return the result as a :class:`~wolframclient.evaluation.result.WolframEvaluationJSONResponse`.
        """
        return self._call_evaluation_api(self._normalize_input(expr, **kwargs))

    def function(self, func):
        """Return a `callable` cloud function.

        The object returned can be applied on arguments as any other Python function, and
        is evaluated in the cloud as a Wolfram Language expression using the current cloud
        session.
        """
        return CloudFunction(self, func)

    def __repr__(self):
        return '<{}:base={}, authorized={}>'.format(
            self.__class__.__name__, self.server.cloudbase, self.authorized())


class WolframCloudSessionAsync(WolframCloudSession):
    """ A Wolfram cloud session that call issue asynchronous call.

    Contrary to :class:`~wolframclient.evaluation.WolframCloudSession`, this
    class must be terminated when no more used.

    `max_workers` can be specified and is passed to the ThreadPoolExecutor.
    """

    def __init__(self,
                 credentials=None,
                 server=WolframPublicCloudServer,
                 max_workers=None):
        super(WolframCloudSessionAsync, self).__init__(credentials, server)
        self.thread_pool_exec = None
        self._max_workers = max_workers

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.terminate()

    def terminate(self):
        if self.thread_pool_exec is not None:
            self.thread_pool_exec.shutdown(wait=True)

    def _thread_pool_exec(self):
        if self.thread_pool_exec is None:
            try:
                self.thread_pool_exec = futures.ThreadPoolExecutor(
                    max_workers=self._max_workers)
            except ImportError:
                logger.fatal('Module concurrent.futures is missing.')
                raise NotImplementedError(
                    'Asynchronous evaluation is not available for this Python interpreter.'
                )
        return self.thread_pool_exec

    def call_async(self,
                   api,
                   input_parameters={},
                   target_format='wl',
                   permissions_key=None,
                   **kwargv):
        """Call a given API asynchronously. Returns a :class:`concurrent.futures.Future` object.

        This method requires :mod:`concurrent.futures` which was introduced in `Python 3.2`.
        See :func:`WolframCloudSession.call` for more details about input parameters.

        .. warning::
            Asynchronous evaluation is only available for `Python 3.2` and above.
        """
        return self._thread_pool_exec().submit(
            self.call,
            api,
            input_parameters=input_parameters,
            target_format=target_format,
            permissions_key=permissions_key,
            **kwargv)

    def evaluate_async(self, expr):
        """Send `expr` to the cloud for asynchronous evaluation.

        Returns a
        :class:`concurrent.futures.Future` object.

        `expr` can be a Python object serializable by :func:`~wolframclient.serializers.export`,
        or a the string InputForm of an expression to evaluate.

        .. warning::
            Asynchronous evaluation is only available for `Python 3.2` and above.
        """
        return self._thread_pool_exec().submit(self._call_evaluation_api,
                                               self._normalize_input(expr))

    def function(self, func, asynchronous=False):
        """Return a `callable` cloud function.

        The object returned can be applied on arguments as any other Python function, and
        is evaluated in the cloud as a Wolfram Language expression using the current cloud
        session.

        .. warning::
            Asynchronous evaluation is only available for `Python 3.2` and above.
        """
        return CloudFunction(self, func, asynchronous=asynchronous)


class WolframAPICall(object):
    """Perform an API call to a given target.

    The API call is actually performed when :func:`~wolframclient.evaluation.WolframAPICall.perform`
    is called.

    Parameters can be added using one of the various functions that this class exposes. They
    can be of many types including: string, files, WL serializable python objects, binary data with arbitrary
    content-type (e.g: *image/png*).
    """

    def __init__(self, target, api, permission_key=None):
        self.target = target
        self.api = api
        self.parameters = {}
        self.files = {}
        self.permission_key = permission_key
        self.multipart = False

    def add_parameter(self, name, value):
        """Add a new API input parameter from a serialization python object."""
        self.parameters[name] = value
        return self

    def add_file_parameter(self, name, fp, content_type=None):
        """Add a new API input parameter from a file pointer `fp`"""
        if content_type is None:
            self.files[name] = fp
        else:
            self.files[name] = ('tmp_%s' % name, fp, content_type)
        return self

    def add_binary_parameter(self,
                             name,
                             data,
                             content_type='application/octet-stream'):
        """Add a new API input parameter as a blob of binary data."""
        if not isinstance(data, six.binary_type):
            raise TypeError('Input data by bytes.')
        self.files[name] = ('tmp_%s' % name, data, content_type)
        return self

    def add_image_data_parameter(self,
                                 name,
                                 image_data,
                                 content_type='image/png'):
        """Add a new API image input parameter from binary data.

        If the data in `image_data` does not represent an image in the `PNG` format, the
        optional parameter `content_type` must be set accordingly to the appropriate content
        type.
        e.g: *image/jpeg*, *image/gif*, etc.
        """
        if not isinstance(image_data, six.binary_type):
            raise TypeError('Input data must by bytes.')
        self.files[name] = ('tmp_image_%s' % name, image_data, content_type)
        return self

    def perform(self, **kwargs):
        """Make the API call, return the result."""
        return self.target.call(
            self.api,
            input_parameters=self.parameters,
            files=self.files,
            permissions_key=self.permission_key,
            **kwargs)

    def __repr__(self):
        return 'WolframAPICall<api=%s>' % (self.api, )

    def __str__(self):
        return repr(self)


class CloudFunction(object):

    __slots__ = 'session', 'wlfunc', 'evaluation_func'

    def __init__(self, session, func, asynchronous=False):
        self.session = session
        if isinstance(func, six.string_types) or isinstance(
                func, six.binary_type):
            self.wlfunc = wl.ToExpression(func)
        else:
            self.wlfunc = func
        if asynchronous:
            self.evaluation_func = session.__class__.evaluate_async
        else:
            self.evaluation_func = session.__class__.evaluate

    def __call__(self, *args, **kwargs):
        return self.evaluation_func(self.session,
                                    wl.Construct(self.wlfunc, *args, **kwargs))

    def __repr__(self):
        return 'CloudFunction<function=%s>' % (self.wlfunc)


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


# def url_join(*fragments):
#     """ Join fragments of a URL, dealing with slashes."""
#     if len(fragments) == 0:
#         return ''
#     buff = []
#     for fragment in fragments:
#         stripped = fragment.strip('/')
#         if len(stripped) > 0:
#             buff.append(stripped)
#             buff.append('/')

#     last = fragments[-1]
#     # add a trailing '/' if present.
#     if len(last) > 0 and last[-1] != '/':
#         buff.pop()
#     return ''.join(buff)
