# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import logging

from wolframclient.deserializers import binary_deserialize
from wolframclient.evaluation.cloud.request_adapter import wrap_response
from wolframclient.exception import (
    RequestException, WolframEvaluationException, WolframLanguageException)
from wolframclient.utils import six
from wolframclient.utils.api import json
from wolframclient.utils.decorators import cached_property

logger = logging.getLogger(__name__)

__all__ = [
    'WolframResult', 'WolframAPIResponseBuilder', 'WolframAPIResponse',
    'WolframEvaluationJSONResponse', 'WolframKernelEvaluationResult'
]


class WolframResultBase(object):
    pass


class WolframResult(WolframResultBase):
    """Most generic result object.

    The actual result is returned via method :func:`~wolframclient.evaluation.result.WolframResult.get`.
    If the result is a `success`, the field `result` is returned otherwise `failure` is returned and most
    likely contains an error message.
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
        if self.messages:
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

    def get(self):
        """Return the result or raise an exception."""
        if not self._built:
            self.build()
        if self.success or self._is_message_failure:
            return self.result
        else:
            raise WolframEvaluationException(
                'Evaluation failed.', result=self.result, messages=self.failure)

    def parse_response(self):
        """ Parse the result input and set the attribute `parsed_response`.

        The result input can be encoded in various formats such as WXF, JSON, etc.
        The `parsed_response` dict is expected to have keys corresponding to those of the association returned by
        :wl:`EvaluationData`.
        """
        raise NotImplementedError(
            '%s does not implement parse_response method.' %
            (self.__class__.__name__,))

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
            if self._failure  == 'MessageFailure':
                self._is_message_failure = True
                self._messages_name = self.parsed_response['Messages']
                self._messages = self.parsed_response['MessagesText']
            else:
                logger.warning('Evaluation failed.')
                for msg in self.parsed_response.get('MessagesText', []):
                    logger.warning(msg)
                self._is_message_failure = False

        self._built = True

    def __repr__(self):
        if self.success:
            return '{}<expression={}>'.format(
                self.__class__.__name__, self.result)
        elif self.is_message_failure:
            return '{}<success={}, result={}, messages={}>'.format(
                self.__class__.__name__, self.success, self.result, self.messages)
        else:
            return '{}<failure={}>'.format(
                self.__class__.__name__, self.failure)

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


class WolframEvaluationResponse(WolframEvaluationResultBase):
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

    def build(self):
        if not self.request_error:
            super().build()
        else:
            logger.fatal('Server invalid response %i: %s',
                         self.http_response.status(),
                         self.http_response.text())
            raise RequestException(self.http_response)



class WolframEvaluationWXFResponse(WolframEvaluationResponse):
    """ Result object associated with cloud evaluation request WXF encoded. """

    def parse_response(self):
        wxf = self.http_response.content()
        try:
            self.parsed_response = binary_deserialize(wxf)
        except WolframLanguageException as e:
            self.build_invalid_format(response_format_name='WXF')


class WolframEvaluationJSONResponse(WolframEvaluationResponse):
    """ Result object associated with cloud evaluation request JSON encoded. """

    def parse_response(self):
        try:
            self.parsed_response = self.http_response.json()
        except json.JSONDecodeError as e:
            self.build_invalid_format(response_format_name='JSON')


class WolframEvaluationResponseAsync(WolframEvaluationResponse):
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

    async def get(self):
        """Return the result or raise an exception based on the success status."""
        if not self._built:
            await self.build()
        if self._success or self._is_message_failure:
            return self._result
        else:
            raise WolframEvaluationException(
                'Cloud evaluation failed.', messages=self._failure)


class WolframEvaluationJSONResponseAsync(WolframEvaluationResponseAsync):
    """Asynchronous result object associated with cloud evaluation request encoded with JSON. """

    async def parse_response(self):
        try:
            self.parsed_response = await self.http_response.json()
        except json.JSONDecodeError as e:
            self.build_invalid_format(response_format_name='JSON')


class WolframEvaluationWXFResponseAsync(WolframEvaluationResponseAsync):
    """Asynchronous result object associated with cloud evaluation request encoded with WXF. """

    async def parse_response(self):
        wxf = await self.http_response.content()
        try:
            self.parsed_response = binary_deserialize(wxf)
        except WolframLanguageException as e:
            self.build_invalid_format(response_format_name='WXF')


class WolframAPIResponse(WolframResult):
    """Generic API response."""

    def __init__(self, response, decoder=None):
        self.response = response
        self.decoder = decoder
        self.content_type = response.headers().get('Content-Type', None)
        self.status = response.status()
        self.json = None
        self.success = None
        self._built = False

    def _iter_error(self):
        if not self.success and self.json:
            for field, err in self.json.get('Fields', {}).items():
                failure = err.get('Failure', None)
                if failure is not None:
                    yield (field, failure)

    def _iter_full_error_report(self):
        if not self.success and self.json:
            yield from self.json.get('Fields', {})

    def build(self):
        raise NotImplementedError

    def get(self):
        if not self._built:
            self.build()
        return super().get()

    def iter_error(self):
        """Generator of tuples made from the field name and the associated error message"""
        if not self._built:
            self.build()
        yield from self._iter_error()

    def iter_full_error_report(self):
        """Generator of tuples made from the field name and the associated entire error report."""
        if not self._built:
            self.build()
        yield from self._iter_full_error_report()

    def fields_in_error(self):
        """Return all the fields in error with their message as a list of tuples"""
        return list(self.iter_error())

    def error_report(self):
        """Return all the fields in error with their report as a list of tuples."""
        return list(self.iter_full_error_report())

    def __repr__(self):
        return '<%s:success=%s>' % (self.__class__.__name__, self.success)


class WolframAPIResponseAsync(WolframAPIResponse):
    async def get(self):
        await self.build()
        return super(WolframAPIResponse, self).get()

    async def build(self):
        raise NotImplementedError

    async def iter_error(self):
        raise NotImplementedError

    async def iter_full_error_report(self):
        raise NotImplementedError

    async def fields_in_error(self):
        """Return all the fields in error with their message as a list of tuples"""
        if not self._built:
            await self.build()
        return list(self._iter_error())

    async def error_report(self):
        """Return all the fields in error with their report as a list of tuples."""
        if not self._built:
            await self.build()
        return list(self._iter_full_error_report())


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
        self.failure = None

    def build(self):
        self._built = True
        if self.decoder is not None:
            try:
                self.result = self.decoder(self.response.content())
            except Exception as e:
                self.success = False
                self.failure = 'Decoder error: {}'.format(e)
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
                self.failure = 'Decoder error: {}'.format(e)
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
        self.failure = 'Resource permanently moved to new location {}'.format(
            self.location)


class WolframAPIResponse301Async(WolframAPIResponse301,
                                 WolframAPIResponseAsync):
    pass


class WolframAPIResponse302(WolframAPIResponseRedirect):
    def _specific_failure(self):
        # hack because the server is not returning 403. cf. CLOUD-12946
        if self.location is not None and 'j_spring_oauth_security_check' in self.location:
            self.failure = 'Not allowed to access requested resource.'
        else:
            self.failure = 'Resource moved to new location {}'.format(
                self.location)


class WolframAPIResponse302Async(WolframAPIResponse302,
                                 WolframAPIResponseAsync):
    pass


class WolframAPIResponse400(WolframAPIFailureResponse):
    def build(self):
        # ignoring content-type. Must be JSON. Make sure it's robust enough.
        try:
            self.json = self.response.json()
        except json.JSONDecodeError as e:
            logger.fatal('Failed to parse server response as json:\n%s',
                         self.response.content())
            raise RequestException(
                self.response, msg='Failed to parse server response as json.')
        self._update_from_json()
        self._built = True

    def _update_from_json(self):
        self.failure = self.json.get('Failure', None)
        fields = self.json.get('Fields', None)
        logger.warning('Wolfram API error response: %s', self.failure)
        if fields is not None:
            self._fields_in_error = set(fields.keys())
            logger.warning('Fields in error: %s', self._fields_in_error)


class WolframAPIResponse400Async(WolframAPIResponse400,
                                 WolframAPIResponseAsync):
    async def build(self):
        # ignoring content-type. Must be JSON. Make sure it's robust enough.
        try:
            self.json = await self.response.json()
        except json.JSONDecodeError as e:
            logger.fatal('Failed to parse server response as json:\n%s', await
                         self.response.content)
            raise RequestException(
                self.response, msg='Failed to parse server response as json.')
        self._update_from_json()
        self._built = True


class WolframAPIResponse401(WolframAPIFailureResponse):
    def build(self):
        # ignoring content-type. Must be JSON. Make sure it's robust enough.
        self.failure = self.response.text()
        logger.warning('Authentication missing or failed. Server response: %s',
                       self.failure)
        self._built = True


class WolframAPIResponse401Async(WolframAPIResponse401,
                                 WolframAPIResponseAsync):
    async def build(self):
        # ignoring content-type. Must be JSON. Make sure it's robust enough.
        self.failure = await self.response.text()
        self._built = True
        logger.warning('Authentication missing or failed. Server response: %s',
                       self.failure)


class WolframAPIResponse404(WolframAPIFailureResponse):
    def build(self):
        self.failure = "The resource %s can't not be found." % self.response.url(
        )
        logger.warning('Wolfram API error response: %s', self.failure)
        self._built = True


class WolframAPIResponse404Async(WolframAPIResponse404,
                                 WolframAPIResponseAsync):
    async def build(self):
        WolframAPIResponse404.build(self)


class WolframAPIResponseGeneric(WolframAPIFailureResponse):
    async def build(self):
        self.failure = self.response.text()
        self._built = True


class WolframAPIResponseGenericAsync(WolframAPIResponseAsync):
    async def build(self):
        self.failure = await self.response.text()
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
