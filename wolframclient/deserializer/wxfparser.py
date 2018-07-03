# -*- coding: utf-8 -*-
from __future__ import absolute_import, print_function, unicode_literals
from wolframclient.utils.six import BytesIO, binary_type, string_types
from wolframclient.serializers.wxfencoder import wxfexpr
from wolframclient.serializers.wxfencoder.serializer import SerializationContext, WXF_HEADER_COMPRESS, WXF_HEADER_SEPARATOR, WXF_VERSION
from wolframclient.exception import WolframLanguageException

class WolframParserException(WolframLanguageException):
    pass

def binary_deserialize(wxf_input, enforce=True):
    """Deserialize binary data, yield instances of WXFExpr.

    `data` can be a string of bytes with the serialized expression, a string of unicodes
    in which case it is considered as a filename, a object implementing a `read` method.
    """
    if enforce:
        self._context = SerializationContext()
    else:
        self._context = NoEnforcingContext()

    if isinstance(wxf_input, string_types):
        with open(wxf_input, 'rb') as fp:
            for o in _deserialize(fp):
                yield o
    elif isinstance(input, binary_type):
        o = _deserialize(BytesIO(wxf_input))
        for o in _deserialize(fp):
            yield o
    elif hasattr(wxf_input, 'read'):
        o = _deserialize(wxf_input)
        for o in _deserialize(fp):
            yield o
    else:
        raise TypeError('WXF input must be a string of bytes, a filename as string, a readable object. Got %s.' %
                        wxf_input.__class__.__name__)

    
def _deserialize(reader):
    version, compress = _parse_header(reader)

def _parse_header(reader):
    compress = False
    next_byte = reader.read(1)
    if next_byte == WXF_VERSION:
        next_byte = reader.read(1)
    else:
        raise WolframParserException('WXF version %s is not supported.' % version)
    if next_byte == WXF_HEADER_COMPRESS:
        compress = True
        next_byte = reader.read(1)
    if next_byte != WXF_HEADER_SEPARATOR:
        raise WolframParserException('Invalid header. Failed to find header separator \':\'.')
    return (version, compress)
    
