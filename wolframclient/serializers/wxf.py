from __future__ import absolute_import, print_function, unicode_literals

from itertools import chain, starmap

from wolframclient.serializers.base import FormatSerializer
from wolframclient.serializers.utils import py_encode_decimal, safe_len
from wolframclient.serializers.wxfencoder.constants import (
    WXF_CONSTANTS,
    WXF_HEADER_COMPRESS,
    WXF_HEADER_SEPARATOR,
    WXF_VERSION,
)
from wolframclient.serializers.wxfencoder.utils import (
    float_to_bytes,
    integer_size,
    integer_to_bytes,
    numeric_array_to_wxf,
    packed_array_to_wxf,
    varint_bytes,
)
from wolframclient.utils import six
from wolframclient.utils.api import zlib
from wolframclient.utils.encoding import concatenate_bytes, force_bytes, force_text
from wolframclient.utils.functional import partition


def serialize_rule(key, value, sep=(WXF_CONSTANTS.Rule,)):
    return chain(sep, key, value)


def get_length(iterable, length=None):
    if length is not None:
        return iterable, length

    length = safe_len(iterable)

    if length is not None:
        return iterable, length

    iterable = tuple(iterable)

    return iterable, len(iterable)


def compress(data):

    compressor = zlib.compressobj()

    for token in map(compressor.compress, map(concatenate_bytes, partition(data, 100))):
        yield token

    yield compressor.flush()


class WXFSerializer(FormatSerializer):
    """ Serialize python objects to WXF. """

    def __init__(self, normalizer=None, compress=False, **opts):
        super(WXFSerializer, self).__init__(normalizer=normalizer, **opts)
        self.compress = compress

    def generate_bytes(self, data):

        if self.compress:

            return chain(
                (WXF_VERSION, WXF_HEADER_COMPRESS, WXF_HEADER_SEPARATOR),
                compress(self.encode(data)),
            )

        return chain((WXF_VERSION, WXF_HEADER_SEPARATOR), self.encode(data))

    def serialize_symbol(self, name):
        yield WXF_CONSTANTS.Symbol
        yield varint_bytes(len(name))
        yield force_bytes(name)

    def serialize_function(self, head, args, **opts):

        iterable, length = get_length(args, **opts)

        return chain(
            (WXF_CONSTANTS.Function, varint_bytes(length)), head, chain.from_iterable(iterable)
        )

    # numeric
    def serialize_int(self, number):
        try:
            wxf_type, int_size = integer_size(number)
            yield wxf_type
            yield integer_to_bytes(number, int_size)

        except ValueError:
            # WXFExprInteger is raising a ValueError if the integer is not in the appropriate bounds.
            # that check needs to be done in case, it's better to do it only once.

            number = b"%i" % number

            yield WXF_CONSTANTS.BigInteger
            yield varint_bytes(len(number))
            yield number

    def serialize_float(self, number):
        yield WXF_CONSTANTS.Real64
        yield float_to_bytes(number)

    def serialize_decimal(self, number):
        number = py_encode_decimal(number)
        yield WXF_CONSTANTS.BigReal
        yield varint_bytes(len(number))
        yield number

    # text / bytes

    def serialize_string(self, string):
        string = force_bytes(string)
        yield WXF_CONSTANTS.String
        yield varint_bytes(len(string))
        yield string

    def serialize_bytes(self, bytes, as_byte_array=not six.PY2):
        if as_byte_array:
            return (WXF_CONSTANTS.BinaryString, varint_bytes(len(bytes)), bytes)
        else:
            return self.serialize_string(force_text(bytes, encoding="iso8859-1"))

    def serialize_mapping(self, keyvalue, **opts):
        # the normalizer is always sending an generator key, value

        iterable, length = get_length(keyvalue, **opts)

        return chain(
            (WXF_CONSTANTS.Association, varint_bytes(length)),
            chain.from_iterable(starmap(serialize_rule, iterable)),
        )

    def serialize_numeric_array(self, data, dimensions, wl_type):
        return numeric_array_to_wxf(data, dimensions, wl_type)

    def serialize_packed_array(self, data, dimensions, wl_type):
        return packed_array_to_wxf(data, dimensions, wl_type)
