# -*- coding: utf-8 -*-
from __future__ import absolute_import, print_function, unicode_literals
from wolframclient.utils.six import BytesIO, binary_type, string_types, integer_types
from wolframclient.serializers.wxfencoder import wxfexpr
from wolframclient.serializers.wxfencoder.serializer import SerializationContext, WXF_HEADER_COMPRESS, WXF_HEADER_SEPARATOR, WXF_VERSION
from wolframclient.serializers.wxfencoder.streaming import ZipCompressedReader, ExactSizeReader
from wolframclient.exception import WolframParserException
from wolframclient.deserializers.wxf.wxfconsumer import WXFConsumerNumpy, WXFConsumer

class WXFParser(object):
    """Parse a WXF input.

    This class is initialized with a WXF input, and exposes a generator of 
    :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken`. 
    The input `wxf_input` can be a string of bytes with the serialized expression, a string of unicodes
    in which case it is considered as a filename, a object implementing a `read` method.
    
    The generator outputs WXF tokens one by one::

        with open('/tmp/data.wxf', 'rb') as fp:
            parser = WXFParser(fp)
            gen = parser.tokens()
            print(next(gen))

    This low level class is providing intermediary objects to ease the parsing of WXF. Most of
    the time one should directly use high level interface such as 
    :func:`~wolframclient.deserializers.wxf.wxfparser.binary_deserialize`.

    The token generator is generally consumed by an instance of 
    :class:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer`.
    """
    def __init__(self, wxf_input):
        """WXF parser returning Python object from a WXF encoded byte sequence.
        """
        self.context = SerializationContext()
        if isinstance(wxf_input, binary_type):
            self.reader = BytesIO(wxf_input)
        elif hasattr(wxf_input, 'read'):
            self.reader = wxf_input
        else:
            raise TypeError('Class %s neither implements a read method nor is a binary type.' %
                            wxf_input.__class__.__name__)
        version, compress = self.parse_header()
        if compress == True:
            self.reader = ZipCompressedReader(self.reader)
        else:
            self.reader = ExactSizeReader(self.reader)
    
    def tokens(self):
        """Generate instances :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` from a WXF input."""
        yield self.next_token()
        while not self.context.is_valid_final_state():
            yield self.next_token()

    def parse_header(self):
        compress = False
        next_byte = self.reader.read(1)
        if next_byte == WXF_VERSION:
            version = int(next_byte)
            next_byte = self.reader.read(1)
        else:
            raise WolframParserException(
                'Invalid version %s.' % next_byte)
        if next_byte == WXF_HEADER_COMPRESS:
            compress = True
            next_byte = self.reader.read(1)
        if next_byte != WXF_HEADER_SEPARATOR:
            raise WolframParserException(
                'Invalid header. Failed to find header separator \':\'.')
        return (version, compress)

    def parse_array(self, token):
        # Parsing array rank and dimensions
        rank = parse_varint(self.reader)
        if rank == 0:
            raise WolframParserException('Array rank cannot be zero.')
        token.dimensions = []
        for i in range(rank):
            dim = parse_varint(self.reader)
            if dim == 0:
                raise WolframParserException('Array dimensions cannot be zero.')
            token.dimensions.append(dim)
        # reading values
        bytecount = wxfexpr.ARRAY_TYPES_ELEM_SIZE[token.array_type] * token.element_count
        token.data = self.reader.read(bytecount)

    def next_token(self):
        next_byte = self.reader.read(1)
        token = WXFToken(next_byte)
        if next_byte == wxfexpr.WXF_CONSTANTS.Symbol or next_byte == wxfexpr.WXF_CONSTANTS.String or next_byte == wxfexpr.WXF_CONSTANTS.BigInteger or next_byte == wxfexpr.WXF_CONSTANTS.BigReal:
            self.context.add_part()
            token.length = parse_varint(self.reader)
            if token.length == 0:
                token.data = ''
            else:
                token.data = self.reader.read(token.length).decode('utf8')
        elif next_byte == wxfexpr.WXF_CONSTANTS.Integer8:
            self.context.add_part()
            token.data = wxfexpr.StructInt8LE.unpack(self.reader.read(1))[0]
        elif next_byte == wxfexpr.WXF_CONSTANTS.Integer64:
            self.context.add_part()
            token.data = wxfexpr.StructInt64LE.unpack(self.reader.read(8))[0]
        elif next_byte == wxfexpr.WXF_CONSTANTS.Integer32:
            self.context.add_part()
            token.data = wxfexpr.StructInt32LE.unpack(self.reader.read(4))[0]
        elif next_byte == wxfexpr.WXF_CONSTANTS.Integer16:
            self.context.add_part()
            token.data = wxfexpr.StructInt16LE.unpack(self.reader.read(2))[0]
        elif next_byte == wxfexpr.WXF_CONSTANTS.Real64:
            self.context.add_part()
            token.data = wxfexpr.StructDouble.unpack(self.reader.read(8))[0]
        elif next_byte == wxfexpr.WXF_CONSTANTS.Function:
            token.length = parse_varint(self.reader)
            self.context.step_into_new_function(token.length)
        elif next_byte == wxfexpr.WXF_CONSTANTS.Association:
            token.length = parse_varint(self.reader)
            self.context.step_into_new_assoc(token.length)
        elif next_byte == wxfexpr.WXF_CONSTANTS.Rule or next_byte == wxfexpr.WXF_CONSTANTS.RuleDelayed:
            if not self.context.is_rule_valid():
                raise WolframParserException('Rule and RuleDelayed must be parts of an Association.')
            self.context.step_into_new_rule()
        elif next_byte == wxfexpr.WXF_CONSTANTS.PackedArray:
            self.context.add_part()
            token.array_type = self.reader.read(1)
            if token.array_type not in wxfexpr.VALID_PACKED_ARRAY_TYPES:
                raise WolframParserException('Invalid PackedArray value type: %s' % token.array_type)
            self.parse_array(token)
        elif next_byte == wxfexpr.WXF_CONSTANTS.RawArray:
            self.context.add_part()
            token.array_type = self.reader.read(1)
            if token.array_type not in wxfexpr.ARRAY_TYPES_ELEM_SIZE:
                raise WolframParserException('Invalid RawArray value type: %s' % token.array_type)
            self.parse_array(token)
        elif next_byte == wxfexpr.WXF_CONSTANTS.BinaryString:
            self.context.add_part()
            token.length = parse_varint(self.reader)
            if token.length == 0:
                token.data = b''
            else:
                token.data = self.reader.read(token.length)
        else:
            raise WolframParserException('Unexpected token %s' % next_byte)

        return token


class WXFToken(object):
    """Represent a WXF element, often referred as WXF tokens.
    """
    __slots__ = 'wxf_type', 'array_type', 'length', '_dimensions', '_element_count', 'data'
    
    def __init__(self, wxf_type):
        self.wxf_type = wxf_type
        self._dimensions = None
        self._element_count = None
        self.data = None
        self.length = None
    
    @property
    def element_count(self):
        if self._element_count is None and self._dimensions is not None:
            self._update_element_count()
        return self._element_count
    
    @property
    def dimensions(self):
        return self._dimensions

    @dimensions.setter
    def dimensions(self, value):
        if not isinstance(value, list):
            raise TypeError('Dimensions must be a list of positive integers.')
        self._dimensions = value
        if self._element_count is not None:
            self._update_element_count()

    def _update_element_count(self):
        count = 1
        for dim in self._dimensions:
            count = count * dim
        if not isinstance(count, integer_types) or count <= 0:
            raise TypeError('Dimensions must be strictly positive integers.')
        self._element_count = count

    def __str__(self):
        if self.length is not None:
            return 'WXFToken<%s, data=%s, len=%i>' % (self.wxf_type, self.data, self.length)
        else:
            return 'WXFToken<%s, data=%s>' % (self.wxf_type, self.data)
    

def parse_varint(reader):
    """Parse a readable binary buffer for a positive varint encoded integer."""
    count = 0
    continuation = True
    shift = 0
    length = 0
    # when we read from stream we get a sequence of bytes. Its length is 1
    # except if we reached EOF in which case taking index 0 raises IndexError.
    try:
        while continuation and count < 8:
            count = count + 1
            next_byte = reader.read(1)
            next_byte = ord(next_byte)
            length |= (next_byte & 0x7F) << shift
            shift = shift + 7
            continuation = (next_byte & 0x80) != 0

        if continuation:
            next_byte = reader.read(1)
            next_byte = ord(next_byte)
            next_byte &= 0x7F
            if next_byte == 0:
                raise WolframParserException('Invalid last varint byte.')
            length |= next_byte << shift

        return length
    except IndexError:
        raise EOFError('EOF reached while parsing varint encoded integer.')



