from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.evaluation.cloud.exceptions import EncoderException, DecoderException, InputException
from wolframclient.utils.six import string_types
from wolframclient.utils.encoding import force_text
import json
import logging

logger = logging.getLogger(__name__)

class WolframAPI(object):
    '''Public API don't need authentication step. '''
    __slots__ = 'url', 'input_types', 'result_type', 'public'

    def __init__(self, url, input_types={}, result_type=None, public=False):
        self.url = url
        if not isinstance(input_types, dict):
            raise InputException("Input types must be a dictionary.")
        else:
            for key in input_types.keys():
                if not isinstance(key, string_types):
                    raise InputException("Input types keys must be strings.")
        self.input_types = input_types
        self.result_type = result_type
        self.public = public

    def __str__(self):
        return '<WolframAPI:url=%s, public=%s>' % (self.url, self.public)
class WolframAPIResponse(object):
    __slots__ = 'response', 'decoder', 'success', 'output', 'failure', 'json', '_fields_in_error', 'content_type'

    def __init__(self, response, decoder=None):
        self.response = response
        self.decoder = decoder
        self.content_type = response.headers.get('Content-Type', None)
        self.json = None
        if response.status_code == 200:
            self.success = True
            self.failure = None
            if self.decoder is not None:
                self.output = decoder(response.content)
            else:
                self.output = response.content
        elif response.status_code == 400:
            self.success = False
            # ignoring content-type. Must be JSON. Make sure it's robust enough.
            self.json = response.json()
            self.failure = self.json.get('Failure', None)
            fields = self.json.get('Fields', None)
            if fields is not None:
                self._fields_in_error = set(fields.keys())

            logging.warn('Wolfram API error response: %s', self.failure)
        elif response.status_code == 404:
            self.success = False
            self.failure = "The resource %s can't not be found." % response.url
            logging.warn('Wolfram API error response: %s', self.failure)
        elif response.status_code == 302:
            location = response.headers.get('location', None)
            logging.warn('Redirected to %s.', location)
            self.success = False
            if location is not None and 'j_spring_oauth_security_check' in location:
                self.failure = 'Not allowed to access requested resource.'
            else:
                self.failure = 'Resource moved to new location {}'.format(location)
        else:
            # TODO improve error handling.
            self.success = False
            self.failure = self.response.text

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
