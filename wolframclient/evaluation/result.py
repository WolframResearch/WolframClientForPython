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


class WolframKernelEvaluationResult(WolframResultBase):
    """A Wolfram result with WXF encoded data.

    Messages can be issued during a kernel evaluation. Those are
    stored as `messages`. If any message was returned by the kernel
    then the success status is automatically set to `False`.

    The final Python expression is lazily computed when accessing the
    field `result`. The WXF bytes holding the evaluation result are
    stored in `wxf` and thus can be parsed with a consumized parser if
    necessary.
    """

    def __init__(self, wxf, msgs, consumer=None):
        self.success = len(msgs) == 0
        self.failure = None
        self.messages = msgs
        self.wxf = wxf
        self.consumer = consumer

    @cached_property
    def result(self):
        return binary_deserialize(self.wxf, consumer=self.consumer)

    def get(self):
        """Kernel evaluation never fails even if the evaluated expression is a failure object."""
        return self.result

    def __repr__(self):
        if self.success:
            return '{}<success={}, result={}>'.format(
                self.__class__.__name__, self.success, self.result)
        else:
            # msgs = '\n\t'.join(self.messages)
            return '{}<success={}, result={}, messages={}>'.format(
                self.__class__.__name__, self.success, self.result,
                self.messages)


class WolframEvaluationJSONResponse(WolframResultBase):
    """Result object associated with cloud kernel evaluation.

    The response body associated to this type of result must be json encoded.
    Other fields provide additionnal information. The HTTP response object is
    stored as `http_response` and when HTTP error occured it is stored in `request_error`.
    """

    def __init__(self, response):
        self.http_response = wrap_response(response)
        self.request_error = self.http_response.status() != 200
        self._built = False

    def parse_valid_json(self):
        try:
            self._success = self.json['Success']
            self._result = self.json['Result']
            if not self._success:
                failure_type = self.json['FailureType']
                if failure_type == 'MessageFailure':
                    self.is_kernel_message = True
                    self._failure = self.json['MessagesText']
                else:
                    logger.warning('Evaluation failed.')
                    for msg in self.json.get('MessagesText', []):
                        logger.warning(msg)
                    self.is_kernel_message = False
                    self._failure = failure_type
            self._built = True
        except json.JSONDecodeError as e:
            logger.fatal('Server returned invalid JSON: %s', e)
            self.json = None
            self._success = False
            self._result = None
            self._failure = 'Failed to decode JSON response from server'
            self._built = True

    @property
    def success(self):
        if not self._built:
            self._build()
        return self._success

    @property
    def failure(self):
        if not self._built:
            self._build()
        return self._failure

    @property
    def result(self):
        if not self._built:
            self._build()
        return self._result

    def _build(self):
        if self.http_response.status() == 200:
            self.json = self.http_response.json()
            self.request_error = False
            self.parse_valid_json()
        else:
            logger.fatal('Server invalid response %i: %s',
                         self.http_response.status(),
                         self.http_response.text())
            raise RequestException(self.http_response)

    def get(self):
        """Return the result or raise an exception based on the success status."""
        if not self._built:
            self._build()
        if self.success:
            return self.result
        elif self.is_kernel_message:
            if logger.isEnabledFor(logging.WARNING):
                for msg in self.failure:
                    logger.warning(msg)
            return self.result
        else:
            raise WolframEvaluationException(
                'Cloud evaluation failed.', messages=self.failure)

    def __repr__(self):
        if self.success or not self.request_error:
            return '{}<success={}, expression={}>'.format(
                self.__class__.__name__, self.success, self.result)
        else:
            return '{}<request error {}>'.format(self.__class__.__name__,
                                                 self.http_response.status())


class WolframEvaluationJSONResponseAsync(WolframEvaluationJSONResponse):
    """Result object associated with cloud kernel evaluation.

    The response body associated to this type of result must be json encoded.
    Other fields provide additionnal information. The HTTP response object is
    stored as `http_response` and when HTTP error occured it is stored in `request_error`.
    """

    async def _build(self):
        if not self.request_error:
            self.json = await self.http_response.json()
            self.parse_valid_json()
            self._built = True
        else:
            msg = await self.http_response.text()
            logger.fatal('Server invalid response %i: %s',
                         self.http_response.status(), msg)
            self._built = True
            raise RequestException(self.http_response, msg=msg)

    @property
    async def success(self):
        if not self._built:
            await self._build()
        return self._success

    @property
    async def failure(self):
        if not self._built:
            await self._build()
        return self._failure

    @property
    async def result(self):
        if not self._built:
            await self._build()
        return self._result

    async def get(self):
        """Return the result or raise an exception based on the success status."""
        if not self._built:
            await self._build()
        if self._success:
            return self._result
        elif self.is_kernel_message:
            if logger.isEnabledFor(logging.WARNING):
                for msg in self._failure:
                    logger.warning(msg)
            return self._result
        else:
            raise WolframEvaluationException(
                'Cloud evaluation failed.', messages=self._failure)

    def __repr__(self):
        if self._built and (not self.request_error or self._success):
            return '{}<successful request={}, expression={}>'.format(
                self.__class__.__name__, self._success, self._result)
        elif self.request_error:
            return '{}<request error {}>'.format(self.__class__.__name__,
                                                 self.http_response.status())
        else:
            return '{}<sucessful request, body not read>'.format(
                self.__class__.__name__)


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

    def _build(self):
        raise NotImplementedError

    def get(self):
        if not self._built:
            self._build()
        return super().get()

    def iter_error(self):
        """Generator of tuples made from the field name and the associated error message"""
        if not self._built:
            self._build()
        yield from self._iter_error()

    def iter_full_error_report(self):
        """Generator of tuples made from the field name and the associated entire error report."""
        if not self._built:
            self._build()
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
        await self._build()
        return super(WolframAPIResponse, self).get()

    async def _build(self):
        raise NotImplementedError

    # async generators do not exist in 3.5
    # async def iter_error(self):
    #     """Generator of tuples made from the field name and the associated error message"""
    #     if not self._built:
    #         await self._build()
    #     for err in self._iter_error():
    #         yield err
    # async def iter_full_error_report(self):
    #     """Generator of tuples made from the field name and the associated entire error report."""
    #     if not self._built:
    #         await self._build()
    #     for err in self._iter_full_error_report():
    #         yield err
    async def iter_error(self):
        raise NotImplementedError

    async def iter_full_error_report(self):
        raise NotImplementedError

    async def fields_in_error(self):
        """Return all the fields in error with their message as a list of tuples"""
        if not self._built:
            await self._build()
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

    def _build(self):
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
    async def _build(self):
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

    def _build(self):
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
    def _build(self):
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
    async def _build(self):
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
    def _build(self):
        # ignoring content-type. Must be JSON. Make sure it's robust enough.
        self.failure = self.response.text()
        logger.warning('Authentication missing or failed. Server response: %s',
                       self.failure)
        self._built = True


class WolframAPIResponse401Async(WolframAPIResponse401,
                                 WolframAPIResponseAsync):
    async def _build(self):
        # ignoring content-type. Must be JSON. Make sure it's robust enough.
        self.failure = await self.response.text()
        self._built = True
        logger.warning('Authentication missing or failed. Server response: %s',
                       self.failure)


class WolframAPIResponse404(WolframAPIFailureResponse):
    def _build(self):
        self.failure = "The resource %s can't not be found." % self.response.url(
        )
        logger.warning('Wolfram API error response: %s', self.failure)
        self._built = True


class WolframAPIResponse404Async(WolframAPIResponse404,
                                 WolframAPIResponseAsync):
    async def _build(self):
        WolframAPIResponse404._build(self)


class WolframAPIResponseGeneric(WolframAPIFailureResponse):
    async def _build(self):
        self.failure = self.response.text()
        self._built = True


class WolframAPIResponseGenericAsync(WolframAPIResponseAsync):
    async def _build(self):
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
