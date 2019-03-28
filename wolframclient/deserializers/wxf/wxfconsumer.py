# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import decimal
import re

from wolframclient.exception import WolframParserException
from wolframclient.language.expression import WLFunction, WLSymbol
from wolframclient.serializers.wxfencoder import constants
from wolframclient.utils.api import numpy

__all__ = ['WXFConsumer', 'WXFConsumerNumpy']


class WXFConsumer(object):
    """Map WXF types to Python object generating functions.

    This class exposes a comprehensive list of methods consuming WXF types.
    Subclasses can override these members to implement custom parsing logic.

    Example implementing a consumer that maps any function with head 
    :wl:`DirectedInfinity` to float('inf')::

        class ExampleConsumer(WXFConsumer):
            Infinity = wl.DirectedInfinity
            def build_function(self, head, arg_list, **kwargs):
                if head == self.Infinity:
                    return float('inf')
                else:
                    super().build_function(head, args_list, **kwargs)

    Test the new consumer::

        >>> wxf = export({'-inf': wl.DirectedInfinity(-1), '+inf': wl.DirectedInfinity(1)}, target_format='wxf')
        >>> binary_deserialize(wxf, consumer=ExampleConsumer())
        {'-inf': inf, '+inf': inf}

    Compare with default result::

        >>> binary_deserialize(wxf)
        {'-inf': DirectedInfinity[-1], '+inf': DirectedInfinity[1]}

    Once initialized, the entry point of a consumer is the method
    :func:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer.next_expression`.
    It takes a token generator and returns a Python object. This method is particularly
    useful when building nested expressions, e.g:
    :func:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer.build_function`,
    :func:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer.consume_association`, etc,
    in order to fetch sub-expressions.
    """

    _mapping = {
        constants.WXF_CONSTANTS.Function: 'consume_function',
        constants.WXF_CONSTANTS.Symbol: 'consume_symbol',
        constants.WXF_CONSTANTS.String: 'consume_string',
        constants.WXF_CONSTANTS.BinaryString: 'consume_binary_string',
        constants.WXF_CONSTANTS.Integer8: 'consume_integer8',
        constants.WXF_CONSTANTS.Integer16: 'consume_integer16',
        constants.WXF_CONSTANTS.Integer32: 'consume_integer32',
        constants.WXF_CONSTANTS.Integer64: 'consume_integer64',
        constants.WXF_CONSTANTS.Real64: 'consume_real64',
        constants.WXF_CONSTANTS.BigInteger: 'consume_bigint',
        constants.WXF_CONSTANTS.BigReal: 'consume_bigreal',
        constants.WXF_CONSTANTS.PackedArray: 'consume_packed_array',
        constants.WXF_CONSTANTS.NumericArray: 'consume_numeric_array',
        constants.WXF_CONSTANTS.Association: 'consume_association',
        constants.WXF_CONSTANTS.Rule: 'consume_rule',
        constants.WXF_CONSTANTS.RuleDelayed: 'consume_rule_delayed'
    }

    def next_expression(self, tokens, **kwargs):
        """Deserialize the next expression starting at the next token yield by `tokens`."""
        token = next(tokens)
        consumer = self._consumer_from_type(token.wxf_type)
        return consumer(token, tokens, **kwargs)

    def _consumer_from_type(self, wxf_type):
        try:
            func = self._mapping[wxf_type]
        except KeyError:
            raise WolframParserException(
                'Class %s does not implement any consumer method for WXF token %s'
                % (self.__class__.__name__, wxf_type))
        return getattr(self, func)

    _LIST = WLSymbol('List')

    def consume_function(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *function*.

        Return a :class:`list` if the head is symbol `List`, otherwise returns the result of :func:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer.build_function`
        applied to the head and arguments.

        Usually custom parsing rules target Functions, but not List. To do so, it is recommended to override
        :func:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer.build_function`.
        """
        head = self.next_expression(tokens, **kwargs)
        args = []
        for i in range(current_token.length):
            args.append(self.next_expression(tokens, **kwargs))
        if head == self._LIST:
            return args
        else:
            return self.build_function(head, args, **kwargs)

    def build_function(self, head, arg_list, **kwargs):
        """Create a Python object from head and args.

        This function can be conveniently overloaded to create specific Python objects
        from various heads. e.g: DateObject, Complex, etc.
        """
        return WLFunction(head, *arg_list)

    def consume_association(self,
                            current_token,
                            tokens,
                            dict_class=dict,
                            **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *association*.

        By default, return a :class:`dict` made from the rules.
        The named option `dict_class` can be set to any type in which case an instance of
        :class:`dict_class` is returned.
        """
        return dict_class(
            self.next_expression(tokens, **kwargs)
            for i in range(current_token.length))

    def consume_rule(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *rule* as a tuple"""
        return (self.next_expression(tokens, **kwargs),
                self.next_expression(tokens, **kwargs))

    def consume_rule_delayed(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *rule* as a tuple"""
        return (self.next_expression(tokens, **kwargs),
                self.next_expression(tokens, **kwargs))

    BUILTIN_SYMBOL = {
        'True': True,
        'False': False,
        'Null': None,
        'Indeterminate': float('NaN')
    }
    """ See documentation of :func:`~wolframclient.serializers.encoders.builtin.encode_none` for more information
        about the mapping of None and Null. """

    def consume_symbol(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *symbol* as a :class:`~wolframclient.language.expression.WLSymbol`"""
        try:
            return self.BUILTIN_SYMBOL[current_token.data]
        except KeyError:
            return WLSymbol(current_token.data)

    def consume_bigint(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *big integer* as a :class:`int`."""
        try:
            return int(current_token.data)
        except ValueError:
            raise WolframParserException(
                'Invalid big integer value: %s' % current_token.data)

    BIGREAL_RE = re.compile(r'([^`]+)(`[0-9.]+){0,1}(\*\^[0-9]+){0,1}')

    def consume_bigreal(self, current_token, tokens, **kwargs):
        """Parse a WXF big real as a WXF serializable big real.

        There is not such thing as a big real, in Wolfram Language notation, in Python. This
        wrapper ensures round tripping of big reals without the need of `ToExpression`.
        Introducing `ToExpression` would imply to marshall the big real data to avoid malicious
        code from being introduced in place of an actual real.
        """

        match = self.BIGREAL_RE.match(current_token.data)

        if match:

            num, prec, exp = match.groups()

            if exp:
                return decimal.Decimal('%se%s' % (num, exp[2:]))

            return decimal.Decimal(num)

        raise WolframParserException(
            'Invalid big real value: %s' % current_token.data)

    def consume_string(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *string* as a string of unicode utf8 encoded."""
        return current_token.data

    def consume_binary_string(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *binary string* as a string of bytes."""
        return current_token.data

    def consume_integer8(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *integer* as a :class:`int`."""
        return current_token.data

    def consume_integer16(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *integer* as a :class:`int`."""
        return current_token.data

    def consume_integer32(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *integer* as a :class:`int`."""
        return current_token.data

    def consume_integer64(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *integer* as a :class:`int`."""
        return current_token.data

    def consume_real64(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *real* as a :class:`float`."""
        return current_token.data

    def consume_numeric_array(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *raw array*.

        This method return :class:`list`, and made the assumption that system is little endian.
        """
        return self._array_to_list(current_token, tokens)

    def consume_packed_array(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *packed array*.

        This method return :class:`list`, and made the assumption that system is little endian.
        """
        return self._array_to_list(current_token, tokens)


# memoryview.cast was introduced in Python 3.3.

    if hasattr(memoryview, 'cast'):
        unpack_mapping = {
            constants.ARRAY_TYPES.Integer8: 'b',
            constants.ARRAY_TYPES.UnsignedInteger8: 'B',
            constants.ARRAY_TYPES.Integer16: 'h',
            constants.ARRAY_TYPES.UnsignedInteger16: 'H',
            constants.ARRAY_TYPES.Integer32: 'i',
            constants.ARRAY_TYPES.UnsignedInteger32: 'I',
            constants.ARRAY_TYPES.Integer64: 'q',
            constants.ARRAY_TYPES.UnsignedInteger64: 'Q',
            constants.ARRAY_TYPES.Real32: 'f',
            constants.ARRAY_TYPES.Real64: 'd',
            constants.ARRAY_TYPES.ComplexReal32: 'f',
            constants.ARRAY_TYPES.ComplexReal64: 'd',
        }

        def _to_complex(self, array, max_depth, curr_depth):
            # recursivelly traverse the array until the last (real) dimension is reached
            # it correspond to an array of (fake) array of two elements (real and im parts).
            if curr_depth < max_depth - 1:
                for sub in array:
                    self._to_complex(sub, max_depth, curr_depth + 1)
                return
            # iterate over the pairs
            for index, complex_pair in enumerate(array):
                array[index] = complex(*complex_pair)

        def _array_to_list(self, current_token, tokens):
            view = memoryview(current_token.data)
            if current_token.array_type == constants.ARRAY_TYPES.ComplexReal32 or current_token.array_type == constants.ARRAY_TYPES.ComplexReal64:
                dimensions = list(current_token.dimensions)
                # In the given array, 2 reals give one complex,
                # adding one last dimension to represent it.
                dimensions.append(2)
                as_list = view.cast(
                    self.unpack_mapping[current_token.array_type],
                    shape=dimensions).tolist()
                self._to_complex(as_list, len(current_token.dimensions), 0)
                return as_list
            else:
                return view.cast(
                    self.unpack_mapping[current_token.array_type],
                    shape=current_token.dimensions).tolist()
    else:
        unpack_mapping = {
            constants.ARRAY_TYPES.Integer8: constants.StructInt8LE,
            constants.ARRAY_TYPES.UnsignedInteger8: constants.StructUInt8LE,
            constants.ARRAY_TYPES.Integer16: constants.StructInt16LE,
            constants.ARRAY_TYPES.UnsignedInteger16: constants.StructUInt16LE,
            constants.ARRAY_TYPES.Integer32: constants.StructInt32LE,
            constants.ARRAY_TYPES.UnsignedInteger32: constants.StructUInt32LE,
            constants.ARRAY_TYPES.Integer64: constants.StructInt64LE,
            constants.ARRAY_TYPES.UnsignedInteger64: constants.StructUInt64LE,
            constants.ARRAY_TYPES.Real32: constants.StructFloat,
            constants.ARRAY_TYPES.Real64: constants.StructDouble,
            constants.ARRAY_TYPES.ComplexReal32: constants.StructFloat,
            constants.ARRAY_TYPES.ComplexReal64: constants.StructDouble,
        }

        def _array_to_list(self, current_token, tokens):
            value, _ = self._build_array_from_bytes(
                current_token.data, 0, current_token.array_type,
                current_token.dimensions, 0)
            return value

        def _build_array_from_bytes(self, data, offset, array_type, dimensions,
                                    current_dim):
            new_array = list()
            if current_dim < len(dimensions) - 1:
                for i in range(dimensions[current_dim]):
                    new_elem, offset = self._build_array_from_bytes(
                        data, offset, array_type, dimensions, current_dim + 1)
                    new_array.append(new_elem)
            else:
                struct = self.unpack_mapping[array_type]
                # complex values, need two reals for each.
                if array_type == constants.ARRAY_TYPES.ComplexReal32 or array_type == constants.ARRAY_TYPES.ComplexReal64:
                    for i in range(dimensions[-1]):
                        # this returns a tuple.
                        re = struct.unpack_from(data, offset=offset)
                        offset = offset + struct.size
                        im = struct.unpack_from(data, offset=offset)
                        offset = offset + struct.size
                        new_array.append(complex(re[0], im[0]))
                else:
                    for i in range(dimensions[-1]):
                        # this returns a tuple.
                        value = struct.unpack_from(data, offset=offset)
                        offset = offset + struct.size
                        new_array.append(value[0])
            return new_array, offset


class WXFConsumerNumpy(WXFConsumer):
    """ A WXF consumer that maps WXF array types to NumPy arrays. """

    def consume_array(self, current_token, tokens, **kwargs):
        arr = numpy.frombuffer(
            current_token.data,
            dtype=WXFConsumerNumpy.WXF_TYPE_TO_DTYPE[current_token.array_type])
        arr = numpy.reshape(arr, tuple(current_token.dimensions))
        return arr

    """Build a numpy array from a PackedArray."""
    consume_packed_array = consume_array
    """Build a numpy array from a NumericArray."""
    consume_numeric_array = consume_array

    WXF_TYPE_TO_DTYPE = {
        constants.ARRAY_TYPES.Integer8: 'int8',
        constants.ARRAY_TYPES.Integer16: 'int16',
        constants.ARRAY_TYPES.Integer32: 'int32',
        constants.ARRAY_TYPES.Integer64: 'int64',
        constants.ARRAY_TYPES.UnsignedInteger8: 'uint8',
        constants.ARRAY_TYPES.UnsignedInteger16: 'uint16',
        constants.ARRAY_TYPES.UnsignedInteger32: 'uint32',
        constants.ARRAY_TYPES.UnsignedInteger64: 'uint64',
        constants.ARRAY_TYPES.Real32: 'float32',
        constants.ARRAY_TYPES.Real64: 'float64',
        constants.ARRAY_TYPES.ComplexReal32: 'complex64',
        constants.ARRAY_TYPES.ComplexReal64: 'complex128',
    }
