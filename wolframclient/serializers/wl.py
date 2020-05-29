from __future__ import absolute_import, print_function, unicode_literals

from itertools import chain, starmap

from wolframclient.serializers.base import FormatSerializer
from wolframclient.serializers.utils import py_encode_decimal, py_encode_text
from wolframclient.utils import six
from wolframclient.utils.api import base64
from wolframclient.utils.encoding import force_bytes, force_text


def yield_with_separators(iterable, first, last, separator=b", "):
    yield first
    for i, arg in enumerate(iterable):
        if i:
            yield separator
        for sub in arg:
            yield sub
    yield last


class WLSerializer(FormatSerializer):
    def __init__(self, normalizer=None, indent=None, **opts):
        super(WLSerializer, self).__init__(normalizer=normalizer, **opts)
        self.indent = indent

    def generate_bytes(self, data):
        return self.encode(data)

    def serialize_function(self, head, args, **opts):
        return chain(head, yield_with_separators(args, first=b"[", last=b"]"))

    def serialize_symbol(self, name):
        yield force_bytes(name)

    def serialize_string(self, string):
        return py_encode_text(string)

    def serialize_bytes(self, bytes, as_byte_array=not six.PY2):

        # by default we are serializing as_byte_array for PY3,
        # py2 is by default using strings

        if as_byte_array:
            return self.serialize_function(
                self.serialize_symbol(b"ByteArray"), ((b'"', base64.b64encode(bytes), b'"'),)
            )
        else:
            return self.serialize_string(force_text(bytes, "iso-8859-1"))

    def serialize_decimal(self, number):
        yield py_encode_decimal(number)

    def serialize_float(self, number):
        yield (b"%.13f" % number).rstrip(b"0")

    def serialize_int(self, number):
        yield b"%i" % number

    def serialize_rule(self, lhs, rhs):
        return chain(lhs, (b" -> ",), rhs)

    def serialize_rule_delayed(self, lhs, rhs):
        return chain(lhs, (b" :> ",), rhs)

    def serialize_mapping(self, mapping, **opts):
        return yield_with_separators(
            starmap(self.serialize_rule, mapping), first=b"<|", last=b"|>"
        )

    def serialize_association(self, mapping, **opts):
        return self.serialize_mapping(mapping, **opts)

    def serialize_iterable(self, iterable, **opts):
        return yield_with_separators(iterable, first=b"{", last=b"}")

    def serialize_input_form(self, string):
        yield b"("
        yield force_bytes(string)
        yield b")"
