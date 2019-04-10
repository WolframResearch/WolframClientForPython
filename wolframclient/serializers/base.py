# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import datetime
import inspect
import re
from itertools import chain

from wolframclient.serializers.encoder import Encoder
from wolframclient.serializers.wxfencoder.constants import (
    WXF_HEADER_SEPARATOR, WXF_VERSION)
from wolframclient.serializers.wxfencoder.utils import numeric_array_to_wxf
from wolframclient.utils import six
from wolframclient.utils.api import base64
from wolframclient.utils.encoding import concatenate_bytes, force_text
from wolframclient.utils.functional import first


class FormatSerializer(Encoder):
    def generate_bytes(self, data):
        raise NotImplementedError

    def export(self, data, stream=None):
        if stream:
            if isinstance(stream, six.string_types):
                with open(stream, 'wb') as file:
                    for token in self.generate_bytes(data):
                        file.write(token)
                    return stream

            for token in self.generate_bytes(data):
                stream.write(token)
            return stream

        return concatenate_bytes(self.generate_bytes(data))

    #implementation of several methods

    def serialize_function(self, head, args, **opts):
        raise NotImplementedError

    def serialize_symbol(self, symbol):
        raise NotImplementedError

    def serialize_string(self, obj):
        raise NotImplementedError

    def serialize_float(self, obj):
        raise NotImplementedError

    def serialize_decimal(self, obj):
        raise NotImplementedError

    def serialize_int(self, obj):
        raise NotImplementedError

    def serialize_bytes(self, bytes, as_byte_array = not six.PY2):

        #by default we are serializing as_byte_array for PY3,
        #py2 is by default using strings

        if as_byte_array:
            return self.serialize_function(
                self.serialize_symbol(b'ByteArray'),
                ((b'"', base64.b64encode(bytes), b'"'), ))
        else:
            return self.serialize_string(force_text(bytes, "iso-8859-1"))

    def serialize_input_form(self, string):
        return self.serialize_function(
            self.serialize_symbol(b'ToExpression'),
            (self.serialize_string(string, ), ))

    def serialize_numeric_array(self, data, shape, wl_type):

        payload = concatenate_bytes(
            chain((WXF_VERSION, WXF_HEADER_SEPARATOR),
                  numeric_array_to_wxf(data, shape, wl_type)))

        return self.serialize_function(
            self.serialize_symbol(b'BinaryDeserialize'),
            (self.serialize_bytes(payload, as_byte_array = True), ))

    def serialize_iterable(self, iterable, **opts):
        return self.serialize_function(
            self.serialize_symbol(b'List'), iterable, **opts)

    def serialize_mapping(self, mappable, **opts):
        return self.serialize_function(
            self.serialize_symbol(b'Association'),
            (self.serialize_rule(key, value) for key, value in mappable),
            **opts)

    def serialize_association(self, mappable, **opts):
        return self.serialize_function(
            self.serialize_symbol(b'Association'),
            (self.serialize_rule(key, value) for key, value in mappable),
            **opts)

    def serialize_fraction(self, o):
        return self.serialize_function(
            self.serialize_symbol(b'Rational'), (self.serialize_int(
                o.numerator), self.serialize_int(o.denominator)))

    def serialize_complex(self, o):
        return self.serialize_function(
            self.serialize_symbol(b'Complex'), (
                self.serialize_float(o.real),
                self.serialize_float(o.imag),
            ))

    def serialize_rule(self, lhs, rhs):
        return self.serialize_function(
            self.serialize_symbol(b'Rule'), (lhs, rhs))

    def serialize_rule_delayed(self, lhs, rhs):
        return self.serialize_function(
            self.serialize_symbol(b'RuleDelayed'), (lhs, rhs))

    def serialize_tzinfo(
            self,
            tzinfo,
            date=None,
            name_match=re.compile('^([A-Za-z]+/[A-Za-z]+?|UTC)$')):

        if tzinfo is None:
            return self.serialize_symbol(
                self.target_kernel_version >= 12 and b"None" or b"$TimeZone")

        if name_match:
            name = tzinfo.tzname(None)
            if name and name_match.match(name):
                return self.serialize_string(name)

        return self.serialize_float(
            tzinfo.utcoffset(
                date or datetime.datetime.utcnow()).total_seconds() / 3600)

    def _serialize_external_object(self, o):

        yield "System", "Python"
        yield "Type", "PythonFunction"

        if hasattr(o, '__name__'):
            yield "Name", force_text(o.__name__)
        else:
            yield "Name", force_text(o.__class__.__name__)

        is_module = inspect.ismodule(o)

        yield "IsModule", is_module

        if not is_module:
            module = inspect.getmodule(o)
            if module:
                yield "Module", force_text(module.__name__)

        yield "IsClass", inspect.isclass(o),
        yield "IsFunction", inspect.isfunction(o),
        yield "IsMethod", inspect.ismethod(o),
        yield "IsCallable", callable(o),

        if callable(o):
            try:
                yield "Arguments", map(force_text, first(
                    inspect.getargspec(o)))
            except TypeError:
                #this function can fail with TypeError unsupported callable
                pass

    def serialize_external_object(self, obj):
        return self.serialize_function(
            self.serialize_symbol(
                callable(obj) and b'ExternalFunction' or b'ExternalObject'),
            (self.serialize_mapping(
                (self.encode(key), self.encode(value))
                for key, value in self._serialize_external_object(obj)), ))
