from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.evaluation.cloud.exceptions import EncoderException, DecoderException
# # TODO temporary code
# from os import path
# import sys
# sys.path.append(path.abspath('../WolframClientForPython'))

# from WolframClientForPython.wolframclient.utils import six

import json


class WolframAPI(object):
    '''Public API don't need authentication step. '''
    __slots__ = 'url', 'input_types', 'result_type', 'public'

    def __init__(self, url, *input_types, result_type=None, public=False):
        self.url = url
        self.input_types = input_types
        self.result_type = result_type
        self.public = public

    def __str__(self):
        return '<WolframAPI:url=%s, public=%s>' % (self.url, self.public)
class WolframAPIResponse(object):
    __slots__ = 'api', 'response', 'success', 'output', 'json', '_fields', 'content_type'

    def __init__(self, api, response):
        self.api = api
        self.response = response
        self.content_type = response.headers.get('Content-Type', None)
        if response.status_code == 200:
            self.success = True
            if self.api.result_type is not None:
                self.output = self.api.result_type.output(response.content)
            else:
                self.output = response.content
        elif response.status_code == 400:
            self.success = False
            # ignoring content-type. Must be JSON. Make sure it's robust enough.
            self.json = response.json()
            fields = self.json.get('Fields', None)
            if fields is not None:
                self._fields = set(fields.keys())

        else:
            # TODO improve error handling.
            self.success = False

    def failure(self):
        if self.success:
            return None
        elif self.json is not None:
            return self.json.get('Failure', None)
        else:
            return {'status': self.response.status_code, 'msg': self.response.text}

    def fields(self):
        if self.success or self.json is None:
            return None
        fields = self.json.get('Fields', None)
        if fields is None:
            self._fields = set()
            return self._fields
        else:
            self._fields = set(fields.keys())
            return self._fields

    def iter_error(self):
        for field, err in self.json.get('Fields', {}).items():
            failure = err.get('Failure', None)
            if failure is not None:
                yield (field, failure)

    def fields_in_error(self, field):
       return list(self.iter_error())

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
    

VALID_IMPORT_FORMATS = {'wl', 'json', 'wxf'}

class WolframAPIInput(WolframAPIValue):
    def __init__(self, encoding_format='wl', data_type=None):
        super(WolframAPIInput, self).__init__(encoding_format, data_type)
        if encoding_format == 'json':
            self.encoder = JSONEncoder()
        else:
            # TODO
            self.encoder = None


    @staticmethod
    def _is_valid_format(encoding_format):
        return encoding_format in VALID_IMPORT_FORMATS

    def input(self, parameter_name, data):
        return self.encoder.encode(parameter_name, data)



VALID_EXPORT_FORMATS = {'wl', 'json', 'wxf', 'image'}

class WolframAPIOutput(WolframAPIValue):
    def __init__(self, encoding_format='wl', data_type=None):
        super(WolframAPIOutput, self).__init__(encoding_format, data_type)
        if encoding_format == 'wl':
            self.decode = WLDecoder()
        elif encoding_format == 'json':
            self.decoder = JSONDecoder()
        else:
            # TODO
            raise NotImplementedError('TODO')

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
        return encoding_format in VALID_EXPORT_FORMATS

class FormatEncoder(object):
    def __init__(self):
        raise NotImplementedError()

    def encode(self, parameter_name, data):
        try:
            self._encode(parameter_name, data)
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
            self._decode(data)
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
    def _decode(self, wl_string):
        return wl_string

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
