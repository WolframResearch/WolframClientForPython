# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging

from wolframclient.deserializers import binary_deserialize
from wolframclient.evaluation.cloud.request_adapter import wrap_response
from wolframclient.exception import (
    RequestException, WolframEvaluationException, WolframLanguageException,
    WolframParserException)
from wolframclient.utils import six
from wolframclient.utils.api import json
from wolframclient.utils.decorators import cached_property
from wolframclient.utils.logger import str_trim

logger = logging.getLogger(__name__)

__all__ = [
    'WolframResult', 'WolframAPIResponseBuilder', 'WolframAPIResponse',
    'WolframCloudEvaluationResponse', 'WolframCloudEvaluationWXFResponse', 'WolframCloudEvaluationJSONResponse',
    'WolframKernelEvaluationResult', 'WolframAPIResponseAsync',
    'WolframEvaluationJSONResponseAsync', 'WolframEvaluationWXFResponseAsync'
]


class WolframResultBase(object):
    pass


class WolframResult(WolframResultBase):
    """ The most generic result object.

    The actual result is returned via the method :func:`~wolframclient.evaluation.result.WolframResult.get`. If the
    result is a `success`, the field `result` is returned; otherwise, `failure` is returned and most likely contains an
    error message.
    """

    def __init__(self, result=None, failure=None):
        self.success = failure is None
        self.failure = failure
        self.result = result

    def get(self):
        """Return the result or raise an exception based on the success status."""
        if self.success:
            return self.result
        else:
            raise WolframLanguageException(self.failure)

    def __repr__(self):
        if self.success:
            return '{}<success={}, result={}>'.format(
                self.__class__.__name__, self.success, self.result)
        else:
            return '{}<success={}, failure={}>'.format(
                self.__class__.__name__, self.success, self.failure)


class WolframAsyncResult(WolframResultBase):
    async def get(self):
        raise NotImplementedError


class WolframEvaluationResultBase(WolframResultBase):
    def __init__(self):
        self._built = False
        self._success = False
        self._failure = None
        self._messages = None
        self._messages_name = None
        self._output = None
        self._result = None
        self._is_message_failure = False
        self.parsed_response = None

    @property
    def success(self):
        """ Evaluations succeed when it returns a result and no message is issued. """
        if not self._built:
            self.build()
        return self._success

    @property
    def failure(self):
        if not self._built:
            self.build()
        return self._failure

    @property
    def messages(self):
        """ A list of the messages issued during the evaluation. """
        if not self._built:
            self.build()
        return self._messages

    @property
    def messages_name(self):
        """ A list of the name of all the messages issued during the evaluation. """
        if not self._built:
            self.build()
        return self._messages_name

    def iter_messages(self):
        """
        Iterator over all text messages issued during the evaluation.
        :return: message text as a string.
        """
        if self.messages:
            yield from self.messages

    def iter_messages_name(self):
        if self.messages_name:
            yield from self.messages_name

    def iter_messages_tuple(self):
        """ Iterator over all messages returned as a tuple: (message name, message text)"""
        if self.messages and self.messages_name:
            yield from zip(self.iter_messages_name(), self.iter_messages())

    @property
    def output(self):
        """ A list of all content that got printed during evaluation e.g. using :wl:`Print`. """
        if not self._built:
            self.build()
        return self._output

    def iter_output(self):
        """ Iterator over all printed output."""
        if self.output:
            yield from self.output

    @property
    def result(self):
        if not self._built:
            self.build()
        return self._result

    @property
    def is_message_failure(self):
        if not self._built:
            self.build()
        return self._is_message_failure

    def build(self):
        self.parse_response()
        if self.parsed_response:
            self.build_from_parsed_response()
        # make sure to always build a response, here a generic one.
        elif not self._built:
            self.build_invalid_format()

    def get(self, silent=True):
        """Return the result or raise an exception.

        `silent` can be set to False to log all messages with warning severity. """
        if self.success:
            return self.result
        elif self.is_message_failure:
            if not silent:
                for msg in self.iter_messages():
                    logger.warning(msg)
            return self.result
        else:
            raise WolframEvaluationException(
                'Evaluation failed.', messages=self.failure)

    def parse_response(self):
        """ Parse the result input and set the attribute `parsed_response`.

        The result input can be encoded in various formats such as WXF, JSON, etc.
        The `parsed_response` dict is expected to have keys corresponding to those of the association returned by
        :wl:`EvaluationData`.
        """
        raise NotImplementedError(
            '%s does not implement parse_response method.' %
            (self.__class__.__name__, ))

    def build_invalid_format(self, response_format_name=None):
        """ Build a result object for invalid format cases. """
        logger.fatal('Invalid format. Failed to parse result.')
        self._success = False
        if response_format_name:
            self._failure = 'Failed to decode response encoded with %s' % response_format_name
        else:
            self._failure = 'Failed to decode response.'
        self._built = True

    def build_from_parsed_response(self):
        self._success = self.parsed_response['Success']
        self._result = self.parsed_response['Result']
        self._output = self.parsed_response.get('Output', [])
        if not self._success:
            self._failure = self.parsed_response['FailureType']
            if self._failure == 'MessageFailure':
                self._is_message_failure = True
                self._messages_name = self.parsed_response['Messages']
                self._messages = self.parsed_response['MessagesText']
            else:
                logger.warning('Evaluation failed.')
                self._is_message_failure = False

        self._built = True

    def __repr__(self):
        if self.success:
            return '{}<expression={}>'.format(self.__class__.__name__,
                                              self.result)
        elif self.is_message_failure:
            return '{}<success={}, result={}, messages={}>'.format(
                self.__class__.__name__, self.success, self.result,
                self.messages)
        else:
            return '{}<failure={}>'.format(self.__class__.__name__,
                                           self.failure)


class WolframKernelEvaluationResult(WolframEvaluationResultBase):
    """A Wolfram result with WXF encoded data.

    Messages can be issued during a kernel evaluation. Those are stored as `messages`. If any message was returned by
    the kernel then the success status is `False`.

    The evaluation result is lazily computed when accessing the field `result`. The WXF bytes holding the evaluation
    result are stored in `wxf` and thus can be later parsed with a customized parser if necessary.

    All strings printed during the evaluation (e.g. Print["something"]) are stored in property `output` as a list.
    The dict holding evaluation data is available in `evaluation_data`.
    """

    def __init__(self, wxf_eval_data, consumer=None):
        super().__init__()
        self.wxf_evaluation_data = wxf_eval_data
        # store the expression result serialized.
        self.wxf = None
        self.consumer = consumer

    def parse_response(self):
        self.parsed_response = binary_deserialize(self.wxf_evaluation_data)
        self.wxf = self.parsed_response['Result']

    @cached_property
    def result(self):
        # Kernel evaluation encode the result as WXF. Lazily decoding it using the user consumer.
        return binary_deserialize(super().result, consumer=self.consumer)


class WolframCloudEvaluationResponse(WolframEvaluationResultBase):
    """Result object associated with cloud kernel evaluation.

    The response body associated to this type of result is encoded.
    Other fields provide additional information. The HTTP response object is
    stored as `http_response` and when HTTP error occurred it is stored in `request_error`.
    """

    def __init__(self, response):
        super().__init__()
        self.http_response = wrap_response(response)
        self.request_error = self.http_response.status() != 200

    def __repr__(self):
        if self._built and not self.request_error:
            return super().__repr__()
        elif self.request_error:
            return '{}<request error {}>'.format(self.__class__.__name__,
                                                 self.http_response.status())
        else:
            return '{}<successful request, request body not yet parsed>'.format(
                self.__class__.__name__)

    def get(self, silent=False):
        return super().get(silent)

    def build(self):
        if not self.request_error:
            super().build()
        else:
            logger.fatal('Server invalid response %i: %s',
                         self.http_response.status(),
                         self.http_response.text())
            raise RequestException(self.http_response)


class WolframCloudEvaluationWXFResponse(WolframCloudEvaluationResponse):
    """ Result object associated with cloud evaluation request WXF encoded. """

    def parse_response(self):
        wxf = self.http_response.content()
        try:
            self.parsed_response = binary_deserialize(wxf)
        except WolframLanguageException as e:
            self.build_invalid_format(response_format_name='WXF')


class WolframCloudEvaluationJSONResponse(WolframCloudEvaluationResponse):
    """ Result object associated with cloud evaluation request JSON encoded. """

    def parse_response(self):
        try:
            self.parsed_response = self.http_response.json()
        except json.JSONDecodeError as e:
            self.build_invalid_format(response_format_name='JSON')


class WolframCloudEvaluationResponseAsync(WolframCloudEvaluationResponse):
    """Asynchronous result object associated with cloud evaluation request. """

    async def build(self):
        if not self.request_error:
            await self.parse_response()
            if self.parsed_response:
                self.build_from_parsed_response()
            elif not self._built:
                self.build_invalid_format()
        else:
            msg = await self.http_response.text()
            logger.fatal('Server invalid response %i: %s',
                         self.http_response.status(), msg)
            self._built = True
            raise RequestException(self.http_response, msg=msg)

    async def parse_response(self):
        raise NotImplementedError(
            '%s does not implement parse_response asynchronous method.' %
            (self.__class__.__name__, ))

    @property
    async def success(self):
        if not self._built:
            await self.build()
        return self._success

    @property
    async def failure(self):
        if not self._built:
            await self.build()
        return self._failure

    @property
    async def result(self):
        if not self._built:
            await self.build()
        return self._result

    @property
    async def is_message_failure(self):
        if not self._built:
            await self.build()
        return self.is_message_failure

    @property
    async def messages(self):
        """ A list of the messages issued during the evaluation. """
        if not self._built:
            await self.build()
        return self._messages

    @property
    async def messages_name(self):
        """ A list of the name of all the messages issued during the evaluation. """
        if not self._built:
            await self.build()
        return self._messages_name

    async def iter_messages(self):
        """
        Iterator over all text messages issued during the evaluation.
        :return: message text as a string.
        """
        msgs = await self.messages
        if msgs:
            for msg in msgs:
                yield msg

    async def iter_messages_name(self):
        names = await self.messages_name
        if names:
            for name in names:
                yield name

    async def iter_messages_tuple(self):
        """ Iterator over all messages returned as a tuple: (message name, message text)"""
        msg = await self.messages
        names = await self.messages_name
        if msg and names:
            for tuple_msg in zip(names, msg):
                yield tuple_msg

    @property
    async def output(self):
        """ A list of all content that got printed during evaluation e.g. using :wl:`Print`. """
        if not self._built:
            await self.build()
        return self._output

    async def iter_output(self):
        """ Iterator over all printed output."""
        output = await self.output
        if output:
            for line in self.output:
                yield line

    @property
    async def is_message_failure(self):
        if not self._built:
            await self.build()
        return self._is_message_failure

    async def get(self, silent=False):
        """Return the result or raise an exception based on the success status."""
        if await self.success:
            return await self.result
        elif await self.is_message_failure:
            if not silent:
                for msg in self.iter_messages():
                    logger.warning(msg)
                return self.result
        else:
            raise WolframEvaluationException(
                'Evaluation failed.', messages=self.failure)


class WolframEvaluationJSONResponseAsync(WolframCloudEvaluationResponseAsync):
    """Asynchronous result object associated with cloud evaluation request encoded with JSON. """

    async def parse_response(self):
        try:
            self.parsed_response = await self.http_response.json()
        except json.JSONDecodeError as e:
            self.build_invalid_format(response_format_name='JSON')


class WolframEvaluationWXFResponseAsync(WolframCloudEvaluationResponseAsync):
    """Asynchronous result object associated with cloud evaluation request encoded with WXF. """

    async def parse_response(self):
        wxf = await self.http_response.content()
        try:
            self.parsed_response = binary_deserialize(wxf)
        except WolframLanguageException as e:
            self.build_invalid_format(response_format_name='WXF')


_DEFAULT_DECODERS = {
    'application/vnd.wolfram.wxf': binary_deserialize,
    'application/json': json.loads,
}


class WolframAPIResponse(WolframResult):
    """ A generic API response.

    This class is lazily constructed when the response body becomes available.

    A decoder is inferred from the content type. Currently JSON and WXF formats are supported.
    """

    def __init__(self, response, decoder=None):
        self.response = response
        self.content_type = response.headers().get('Content-Type', None)
        if decoder:
            self.decoder = decoder
        else:
            self.decoder = _DEFAULT_DECODERS.get(self.content_type)
        self.status = response.status()
        self.parsed_response = None
        self.success = None
        self._failure = None
        self._built = False

    def build(self):
        raise NotImplementedError

    def get(self):
        if not self._built:
            self.build()
        return self._get()

    def _get(self):
        if self.success:
            return self.result
        else:
            raise WolframLanguageException(self._failure)

    def failure(self):
        if not self._built:
            self.build()
        return self._failure

    def __repr__(self):
        return '<%s:success=%s>' % (self.__class__.__name__, self.success)


class WolframAPIResponseAsync(WolframAPIResponse):
    """ Asynchronous counterpart of :class:`~wolframclient.evaluation.result.WolframAPIResponse`, awaiting for the
    response body.

    Most of the class logic is implemented in :data:`WolframAPIResponse`, except the build method which has to be a
    coroutine.
    """
    async def get(self):
        """ Return the result or raise an exception based on the success status.

        This is a coroutine."""
        if not self._built:
            await self.build()
        return self._get()

    async def build(self):
        raise NotImplementedError


class WolframAPIFailureResponse(WolframAPIResponse):
    def __init__(self, response, decoder=None):
        super().__init__(response, decoder=decoder)
        self.success = False


class WolframAPIFailureResponseAsync(WolframAPIResponseAsync,
                                     WolframAPIFailureResponse):
    pass


class WolframAPIResponse200(WolframAPIResponse):
    def __init__(self, response, decoder=None):
        super().__init__(response, decoder=decoder)
        self.success = True

    def build(self):
        self._built = True
        if self.decoder is not None:
            try:
                self.result = self.decoder(self.response.content())
            except Exception as e:
                self.success = False
                self._failure = 'Decoder error: {}'.format(e)
                self.exception = e
        else:
            self.result = self.response.content()


class WolframAPIResponse200Async(WolframAPIResponseAsync,
                                 WolframAPIResponse200):
    async def build(self):
        if self.decoder is not None:
            try:
                self.result = self.decoder(await self.response.content())
            except Exception as e:
                self.success = False
                self._failure = 'Decoder error: {}'.format(e)
                self.exception = e
        else:
            self.result = await self.response.content()
        self._built = True


class WolframAPIResponseRedirect(WolframAPIFailureResponse):
    def __init__(self, response, decoder=None):
        super().__init__(response, decoder)
        self.location = None

    def build(self):
        self.location = self.response.headers().get('location', None)
        logger.warning('Redirected to %s.', self.location)
        self._specific_failure()
        self._built = True

    def _specific_failure(self):
        raise NotImplementedError


class WolframAPIResponse301(WolframAPIResponseRedirect):
    def _specific_failure(self):
        ''' should not happen since we follow redirection '''
        self._failure = 'Resource permanently moved to new location {}'.format(
            self.location)


class WolframAPIResponse301Async(WolframAPIResponse301,
                                 WolframAPIResponseAsync):
    pass


class WolframAPIResponse302(WolframAPIResponseRedirect):
    def _specific_failure(self):
        # hack because the server is not returning 403. cf. CLOUD-12946
        if self.location is not None and 'j_spring_oauth_security_check' in self.location:
            self._failure = 'Not allowed to access requested resource.'
        else:
            self._failure = 'Resource moved to new location {}'.format(
                self.location)


class WolframAPIResponse302Async(WolframAPIResponse302,
                                 WolframAPIResponseAsync):
    pass


class WolframAPIResponse400(WolframAPIFailureResponse):
    def __init__(self, response, decoder=None):
        super().__init__(response, decoder=decoder)
        self._fields_in_error = None

    def build(self):
        try:
            if self.decoder:
                self.parsed_response = self.decoder(self.response.content())
            else:
                self.parsed_response = self._unexpected_content_type()
        except (json.JSONDecodeError, WolframParserException):
            logger.fatal('Failed to parse server response as %s:\n%s',
                         self.content_type,
                         str_trim(self.response.content(), max_char=200))
            raise self._failed_to_parse()
        self._update_from_response()
        self._built = True

    def fields_in_error(self):
        """Return all the fields in error with their message as a tuple of tuples"""
        if not self._built:
            self.build()
        return self._fields_in_error

    def _failed_to_parse(self):
        return RequestException(
            self.response, msg='Failed to parse server response.')

    def _unexpected_content_type(self):
        logger.warning(
            'Response content-type: %s is not supported. Cannot decode content: %s',
            self.content_type, str_trim(self.response.content()))
        return {
            'Failure':
            'Cannot decode server response. No decoder found for content-type: %s.'
            % self.content_type
        }

    def _update_from_response(self):
        self._failure = self.parsed_response.get('Failure', None)
        fields = self.parsed_response.get('Fields', None)
        logger.warning('Wolfram API error response: %s', self._failure)
        if fields is not None:
            self._fields = set(fields.keys())
            logger.warning('Fields in error: %s', self._fields)
            self._fields_in_error = []
            for field, err in fields.items():
                failure = err.get('Failure', None)
                if failure is not None:
                    self._fields_in_error.append((field, failure))


class WolframAPIResponse400Async(WolframAPIResponse400,
                                 WolframAPIResponseAsync):
    async def build(self):
        # ignoring content-type. Must be JSON. Make sure it's robust enough.
        try:
            if self.decoder:
                self.parsed_response = self.decoder(await
                                                    self.response.content())
            else:
                self.parsed_response = await self._unexpected_content_type()
        except json.JSONDecodeError as e:
            logger.fatal('Failed to parse server response as %s:\n%s',
                         self.content_type, await self.response.content())
            raise self._failed_to_parse()
        self._update_from_response()
        self._built = True

    async def _unexpected_content_type(self):
        logger.warning(
            'Response content-type: %s is not supported. Cannot decode content: %s',
            self.content_type, str_trim(await self.response.content()))
        return {
            'Failure':
            'Cannot decode server response. No decoder found for content-type: %s.'
            % self.content_type
        }

    async def fields_in_error(self):
        """Return all the fields in error with their message as a list of tuples"""
        if not self._built:
            await self.build()
        return self._fields_in_error


class WolframAPIResponse401(WolframAPIFailureResponse):
    def build(self):
        self._failure = self.response.text()
        logger.warning('Authentication missing or failed. Server response: %s',
                       self._failure)
        self._built = True


class WolframAPIResponse401Async(WolframAPIResponse401,
                                 WolframAPIResponseAsync):
    async def build(self):
        # ignoring content-type. Must be JSON. Make sure it's robust enough.
        self._failure = await self.response.text()
        self._built = True
        logger.warning('Authentication missing or failed. Server response: %s',
                       self._failure)


class WolframAPIResponse404(WolframAPIFailureResponse):
    def build(self):
        self._failure = "The resource %s can't not be found." % self.response.url(
        )
        logger.warning('Wolfram API error response: %s', self._failure)
        self._built = True


class WolframAPIResponse404Async(WolframAPIResponse404,
                                 WolframAPIResponseAsync):
    async def build(self):
        WolframAPIResponse404.build(self)


class WolframAPIResponseGeneric(WolframAPIFailureResponse):
    def build(self):
        self._failure = self.response.text()
        self._built = True


class WolframAPIResponseGenericAsync(WolframAPIResponseAsync):
    async def build(self):
        self._failure = await self.response.text()
        self._built = True


class WolframAPIResponse500(WolframAPIResponseGeneric):
    def __init__(self, response, decoder=None):
        super().__init__(response, decoder)
        logger.fatal('Internal server error occurred.')


class WolframAPIResponse500Async(WolframAPIResponseGenericAsync):
    def __init__(self, response, decoder=None):
        super().__init__(response, decoder)
        logger.fatal('Internal server error occurred.')


class WolframAPIResponseBuilder(object):
    """Map error code to handler building the appropriate
    :class:`~wolframclient.evaluation.result.WolframAPIResponse`
    """
    response_mapper = {
        200: WolframAPIResponse200,
        301: WolframAPIResponse301,
        302: WolframAPIResponse302,
        400: WolframAPIResponse400,
        401: WolframAPIResponse401,
        404: WolframAPIResponse404,
        500: WolframAPIResponse500
    }
    async_response_mapper = {
        200: WolframAPIResponse200Async,
        301: WolframAPIResponse301Async,
        302: WolframAPIResponse302Async,
        400: WolframAPIResponse400Async,
        401: WolframAPIResponse401Async,
        404: WolframAPIResponse404Async,
        500: WolframAPIResponse500Async
    }

    @staticmethod
    def build(response, decoder=None):
        adapter = wrap_response(response)
        if adapter.asynchronous:
            return WolframAPIResponseBuilder.async_response_mapper.get(
                adapter.status(), WolframAPIResponseGenericAsync)(
                    adapter, decoder=decoder)
        else:
            return WolframAPIResponseBuilder.response_mapper.get(
                adapter.status(), WolframAPIResponseGeneric)(
                    adapter, decoder=decoder)

    @staticmethod
    def map(status_code, response_class):
        if not isinstance(response_class, WolframAPIResponse):
            raise ValueError('Response class %s is not a subclass of %s' %
                             (response_class.__class__.__name__,
                              WolframAPIResponse.__class__.__name__))
        if not isinstance(status_code, six.integer_types):
            logger.warning('Invalid status code: %s', status_code)
            raise ValueError('HTTP status code must be string.', )
        logger.debug('Mapping http response status %i to function %s',
                     status_code, response_class.__name__)
        WolframAPIResponseBuilder.response_mapper[status_code] = response_class

    def __init__(self):
        raise NotImplementedError(
            "Cannot initialize. Use static 'method' build.")
