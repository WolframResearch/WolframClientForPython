# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.exception import EvaluationException, RequestException
from wolframclient.utils import six
from wolframclient.utils.api import json
from wolframclient.evaluation.evaluationresult import WolframEvaluationResult
import logging

logger = logging.getLogger(__name__)

__all__ = []

class WolframAPIResponse(object):
    __slots__ = 'response', 'decoder', 'success', 'output', 'failure', 'json', '_fields_in_error', 'content_type', 'exception'

    def __init__(self, response, decoder=None):
        self.response = response
        self.decoder = decoder
        self.content_type = response.headers.get('Content-Type', None)
        self.json = None
        self.success = None
        self._build()

    def _build(self):
        raise NotImplementedError

    def iter_error(self):
        if self.success or self.json is None:
            raise StopIteration
        else:
            for field, err in self.json.get('Fields', {}).items():
                failure = err.get('Failure', None)
                if failure is not None:
                    yield (field, failure)

    def iter_full_error_report(self):
        if self.success or self.json is None:
            raise StopIteration
        else:
            for field, err in self.json.get('Fields', {}).items():
                yield field, err

    def fields_in_error(self):
        return list(self.iter_error())

    def error_report(self):
        return list(self.iter_full_error_report())

    def result(self):
        if self.success:
            return self.output
        else:
            self.failure

    def __repr__(self):
        return '<%s:success=%s>' % (self.__class__.__name__, self.success)


class WolframAPIResponse200(WolframAPIResponse):
    def __init__(self, response, decoder=None):
        super(WolframAPIResponse200, self).__init__(response, decoder)

    def _build(self):
        self.success = True
        self.failure = None
        if self.decoder is not None:
            try:
                decoded_output = self.decoder(self.response.content)
                self.output = decoded_output
            except Exception as e:
                self.success = False
                self.failure = 'Decoder error: {}'.format(e)
                self.exception = e
        else:
            self.output = self.response.content

    # def result(self):
    #     if self.success:
    #         if self.response.encoding
    #             pass
    #     else:
    #         return self.failure

class WolframAPIResponseRedirect(WolframAPIResponse):
    def __init__(self, response, decoder=None):
        super(WolframAPIResponseRedirect, self).__init__(response, decoder)
        self.location = None

    def _build(self):
        self.location = self.response.headers.get('location', None)
        logger.warning('Redirected to %s.', self.location)
        self.success = False
        self._specific_failure()

    def _specific_failure(self):
        raise NotImplementedError

class WolframAPIResponse301(WolframAPIResponseRedirect):
    def __init__(self, response, decoder=None):
        super(WolframAPIResponse301, self).__init__(response, decoder)

    def _specific_failure(self):
        ''' should not happen since we follow redirection '''
        self.failure = 'Resource permanently moved to new location {}'.format(
            self.location)

class WolframAPIResponse302(WolframAPIResponseRedirect):
    def __init__(self, response, decoder=None):
        super(WolframAPIResponse302, self).__init__(response, decoder)

    def _specific_failure(self):
        # hack because the server is not returning 403. cf. CLOUD-12946
        if self.location is not None and 'j_spring_oauth_security_check' in self.location:
            self.failure = 'Not allowed to access requested resource.'
        else:
            self.failure = 'Resource moved to new location {}'.format(self.location)

class WolframAPIResponse400(WolframAPIResponse):
    def __init__(self, response, decoder=None):
        super(WolframAPIResponse400, self).__init__(response, decoder)

    def _build(self):
        self.success = False
        # ignoring content-type. Must be JSON. Make sure it's robust enough.
        try:
            self.json = self.response.json()
        except json.JSONDecodeError as e:
            logger.fatal('Failed to parse server response as json:\n%s', self.response.content)
            raise RequestException(self.response, 'Failed to parse server response as json.')
        self.failure = self.json.get('Failure', None)
        fields = self.json.get('Fields', None)
        logger.warning('Wolfram API error response: %s', self.failure)
        if fields is not None:
            self._fields_in_error = set(fields.keys())
            logger.warning('Fields in error: %s', self._fields_in_error)
        

class WolframAPIResponse401(WolframAPIResponse):
    def __init__(self, response, decoder=None):
        super(WolframAPIResponse401, self).__init__(response, decoder)

    def _build(self):
        self.success = False
        # ignoring content-type. Must be JSON. Make sure it's robust enough.
        self.failure = self.response.text
        logger.warning('Authentication missing or failed. Server response: %s', self.failure)

class WolframAPIResponse404(WolframAPIResponse):
    def __init__(self, response, decoder=None):
        super(WolframAPIResponse404, self).__init__(response, decoder)

    def _build(self):
        self.success = False
        self.failure = "The resource %s can't not be found." % self.response.url
        logger.warning('Wolfram API error response: %s', self.failure)

class WolframAPIResponseGeneric(WolframAPIResponse):
    def __init__(self, response, decoder=None):
        super(WolframAPIResponseGeneric, self).__init__(response, decoder)

    def _build(self):
        self.success = False
        self.failure = self.response.text

class WolframAPIResponse500(WolframAPIResponseGeneric):
    def __init__(self, response, decoder=None):
        super(WolframAPIResponse500, self).__init__(response, decoder)
        logger.fatal('Internal server error occurred.')

class WolframAPIResponseBuilder(object):
    ''' Map error code to handler building the appropriate WolframAPIResponse'''
    response_mapper = {
        200: WolframAPIResponse200,
        301: WolframAPIResponse301,
        302: WolframAPIResponse302,
        400: WolframAPIResponse400,
        401: WolframAPIResponse401,
        404: WolframAPIResponse404,
        500: WolframAPIResponse500
    }

    @staticmethod
    def build(response, decoder=None):
        return WolframAPIResponseBuilder.response_mapper.get(response.status_code, WolframAPIResponseGeneric)(response, decoder=decoder)

    @staticmethod
    def map(status_code, response_class):
        if not isinstance(response_class, WolframAPIResponse):
            raise ValueError('Response class must subclass WolframAPIResponse')
        if not isinstance(status_code, six.integer_types):
            logger.warning('Invalid status code: %s', status_code)
            raise ValueError('HTTP status code must be string.',)
        logger.debug('Mapping http response status %i to function %s', status_code, response_class.__name__)
        WolframAPIResponseBuilder.response_mapper[status_code] = response_class

    def __init__(self):
        raise NotImplementedError("Cannot initialize. Use static 'method' build.")


class WolframEvaluationResponse(WolframEvaluationResult):

    __slots__ = 'http_response', 'json', 'request_error'

    def __init__(self, response):
        self.http_response = response
        if response.status_code == 200:
            self.request_error = False
            try:
                self.json = response.json()
                self.success = self.json['Success']
                self.expr = self.json['Result']
                if not self.success:
                    logger.warning('Evaluation failed: %s',
                                '\n\t'.join(self.json.get('MessagesText', 'Missing field "MessagesText" in response.')))
                    self.failure = self.json['FailureType']
            except json.JSONDecodeError as e:
                logger.fatal('Server returned invalid JSON: %s', e)
                self.json = None
                self.success = False
                self.expr = None
                self.failure = 'Failed to decode JSON response from server'
        else:
            logger.fatal('Server invalid response %i: %s', response.status_code, response.text)
            raise EvaluationException(response)

    def __repr__(self):
        if self.success:
            return '{}<success={}, expr={}>'.format(self.__class__.__name__, self.success, self.expr)
        elif not self.request_error:
            return '{}<success={}, expr={}>'.format(self.__class__.__name__, self.success, self.expr)
        else:
            return '{}<request error {}>'.format(self.__class__.__name__, self.http_response.status_code)
