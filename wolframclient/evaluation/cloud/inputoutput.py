from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.evaluation.cloud.exceptions import EncoderException, DecoderException, InputException
from wolframclient.utils.six import string_types, integer_types
from wolframclient.utils.encoding import force_text
import json
import logging

logger = logging.getLogger(__name__)

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

    def __str__(self):
        return '<WolframAPIResponse:success=%s>' % self.success


class WolframAPIResponse200(WolframAPIResponse):
    def __init__(self, request, decoder=None):
        super(WolframAPIResponse200, self).__init__(request, decoder)

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

class WolframAPIResponseRedirect(WolframAPIResponse):
    def __init__(self, request, decoder=None):
        super(WolframAPIResponseRedirect, self).__init__(request, decoder)
        self.location = None
    
    def _build(self):
        self.location = self.response.headers.get('location', None)
        logger.warning('Redirected to %s.', self.location)
        self.success = False
        self._specific_failure()

    def _specific_failure(self):
        raise NotImplementedError


class WolframAPIResponse301(WolframAPIResponseRedirect):
    def __init__(self, request, decoder=None):
        super(WolframAPIResponse301, self).__init__(request, decoder)

    def _specific_failure(self):
        ''' should not happen since we follow redirection '''
        self.failure = 'Resource permanently moved to new location {}'.format(
            self.location)

class WolframAPIResponse302(WolframAPIResponseRedirect):
    def __init__(self, request, decoder=None):
        super(WolframAPIResponse302, self).__init__(request, decoder)

    def _specific_failure(self):
        # hack because the server is not returning 403. cf. CLOUD-12946
        if self.location is not None and 'j_spring_oauth_security_check' in self.location:
            self.failure = 'Not allowed to access requested resource.'
        else:
            self.failure = 'Resource moved to new location {}'.format(self.location)


class WolframAPIResponse400(WolframAPIResponse):
    def __init__(self, request, decoder=None):
        super(WolframAPIResponse400, self).__init__(request, decoder)

    def _build(self):
        self.success = False
        # ignoring content-type. Must be JSON. Make sure it's robust enough.
        self.json = self.response.json()
        self.failure = self.json.get('Failure', None)
        fields = self.json.get('Fields', None)
        if fields is not None:
            self._fields_in_error = set(fields.keys())
        logger.warning('Wolfram API error response: %s', self.failure)


class WolframAPIResponse404(WolframAPIResponse):
    def __init__(self, request, decoder=None):
        super(WolframAPIResponse404, self).__init__(request, decoder)

    def _build(self):
        self.success = False
        self.failure = "The resource %s can't not be found." % self.response.url
        logger.warning('Wolfram API error response: %s', self.failure)


class WolframAPIResponseGeneric(WolframAPIResponse):
    def __init__(self, request, decoder=None):
        super(WolframAPIResponseGeneric, self).__init__(request, decoder)

    def _build(self):
        self.success = False
        self.failure = self.response.text


class WolframAPIResponse500(WolframAPIResponseGeneric):
    def __init__(self, request, decoder=None):
        super(WolframAPIResponse500, self).__init__(request, decoder)
        logger.fatal('Internal server error occurred.')


class WolframAPIBuilder(object):
    ''' Map error code to handler building the appropriate WolframAPIResponse'''
    response_mapper = {
        200: WolframAPIResponse200,
        301: WolframAPIResponse301,
        302: WolframAPIResponse302,
        400: WolframAPIResponse400,
        404: WolframAPIResponse404,
        500: WolframAPIResponse500
    }

    @staticmethod
    def build(request, decoder=None):
        return WolframAPIBuilder.response_mapper.get(request.status_code, WolframAPIResponseGeneric)(request, decoder=decoder)

    @staticmethod
    def map(status_code, response_class):
        if not isinstance(response_class, WolframAPIResponse):
            raise ValueError('Response class must subclass WolframAPIResponse')
        if not isinstance(status_code, integer_types):
            logger.warning('Invalid status code: %s', status_code)
            raise ValueError('HTTP status code must be string.',)
        logger.debug('Mapping http response status %i to function %s', status_code, response_class.__name__)
        WolframAPIBuilder.response_mapper[status_code] = response_class

    def __init__(self):
        raise NotImplementedError("Cannot initialize. Use static 'method' build.")


class WolframAPIValue(object):
    def __init__(self, encoding_format, data_type=None):
        if not WolframAPIValue._is_valid_format(encoding_format):
            raise ValueError('Invalid encoding format %s' % encoding_format)
        
        self.data_type = data_type
        self.encoding_format = encoding_format

    @staticmethod
    def _is_valid_format(encoding_format):
        return True
    

class FormatEncoder(object):
    def __init__(self):
        raise NotImplementedError()

    def encode(self, parameter_name, data):
        try:
            return self._encode(parameter_name, data)
        except Exception as e:
            raise EncoderException('Failed to encode parameter %s' % parameter_name, e)

    def _encode(self, parameter_name, data):
        raise NotImplementedError()

    @staticmethod
    def from_format(self, encoding_format):
        if(encoding_format == 'wl'):
            return WLEncoder()
        if(encoding_format == 'json'):
            return JSONEncoder()
        else:
            # TODO
            return NotImplementedError('TODO')

class FormatDecoder(object):
    def __init__(self):
        pass

    def decode(self, data):
        try:
            return self._decode(data)
        except Exception as e:
            raise DecoderException(
                'Failed to decode data', e)

    def _decode(self, data):
        raise NotImplementedError()


class JSONEncoder(FormatEncoder):
    def __init__(self):
        pass
    def _encode(self, parameter_name, data):
        return (parameter_name + '__json', json.dumps(data))


class JSONDecoder(FormatDecoder):
    def _decode(self, json_string):
        return json.loads(json_string)


class WLEncoder(FormatEncoder):
    def __init__(self):
        pass

    def _encode(self, parameter_name, data):
        # TODO is it really what we want? if Data is Expr class we need serialization
        return (parameter_name, data)

class WLDecoder(FormatDecoder):
    def _decode(self, wl_content):
        return force_text(wl_content)

class BinaryDecoder(FormatDecoder):
    def _decode(self, data):
        return data


IMPORT_FORMATS = {'wl': WLEncoder(), 'json': JSONEncoder()}


class WolframAPIInput(WolframAPIValue):
    def __init__(self, encoding_format='wl', data_type=None):
        super(WolframAPIInput, self).__init__(encoding_format, data_type)
        self.encoder = IMPORT_FORMATS.get(encoding_format, None)

    @staticmethod
    def _is_valid_format(encoding_format):
        return encoding_format in IMPORT_FORMATS

    def input(self, parameter_name, data):
        return self.encoder.encode(parameter_name, data)


#TODO 'wxf', 'image', 'file'?
OUTPUT_FORMATS = {
    'wl': WLDecoder(), 
    'json': JSONDecoder(),
    'binary': BinaryDecoder()
    }

class WolframAPIOutput(WolframAPIValue):

    def __init__(self, encoding_format='wl', data_type=None):
        super(WolframAPIOutput, self).__init__(encoding_format, data_type)
        self.decoder = OUTPUT_FORMATS.get(encoding_format, None)
        if self.decoder is None:
            # TODO
            raise NotImplementedError('TODO %s' % encoding_format)

    def output(self, data):
        return self.decoder.decode(data)

    # TODO: probably data_type is not required so this could build a singleton and
    # return it.
    @staticmethod
    def json(data_type=None):
        return WolframAPIOutput('json', data_type)

    @staticmethod
    def default(data_type=None):
        return WolframAPIOutput.json()

    @staticmethod
    def _is_valid_format(encoding_format):
        return encoding_format in OUTPUT_FORMATS


# StringType = object()
# IntegerType = object()
# NumericType = object()


# class TypeValidator(object):
#     @staticmethod
#     def is_valid(data_type, data):
#         validator = ValidatorMapping.get(data_type, None)
#         if validator == None:
#             raise ValueError('Unknown data type.')

#         return validator.is_valid(data)

#     def _validate(self, data):
#         raise NotImplementedError()
    

# class _StringValidator(TypeValidator):
#     def _validate(self, data):
#         return isinstance(data, six.string_types)


# class _NumericValidator(TypeValidator):
#     # could be more accurate. Decimal, Complex
#     def _validate(self, data):
#         return isinstance(data, six.integer_types) or isinstance(data, float) or isinstance(data, complex)


# class _IntegerValidator(TypeValidator):
#     def _validate(self, data):
#         return isinstance(data, six.integer_types)


# ValidatorMapping = {
#     StringType: _StringValidator(), 
#     IntegerType: _IntegerValidator(),
#     NumericType: _NumericValidator()
# }
