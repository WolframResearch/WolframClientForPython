from __future__ import absolute_import, print_function, unicode_literals

import decimal
import re

from wolframclient.exception import WolframParserException
from wolframclient.language.expression import WLFunction, WLSymbol
from wolframclient.serializers.wxfencoder import constants
from wolframclient.serializers.wxfencoder.utils import array_to_list
from wolframclient.utils.api import numpy
from wolframclient.utils.datastructures import immutabledict

__all__ = ["WXFConsumer", "WXFConsumerNumpy"]


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
        constants.WXF_CONSTANTS.Function: "consume_function",
        constants.WXF_CONSTANTS.Symbol: "consume_symbol",
        constants.WXF_CONSTANTS.String: "consume_string",
        constants.WXF_CONSTANTS.BinaryString: "consume_binary_string",
        constants.WXF_CONSTANTS.Integer8: "consume_integer8",
        constants.WXF_CONSTANTS.Integer16: "consume_integer16",
        constants.WXF_CONSTANTS.Integer32: "consume_integer32",
        constants.WXF_CONSTANTS.Integer64: "consume_integer64",
        constants.WXF_CONSTANTS.Real64: "consume_real64",
        constants.WXF_CONSTANTS.BigInteger: "consume_bigint",
        constants.WXF_CONSTANTS.BigReal: "consume_bigreal",
        constants.WXF_CONSTANTS.PackedArray: "consume_packed_array",
        constants.WXF_CONSTANTS.NumericArray: "consume_numeric_array",
        constants.WXF_CONSTANTS.Association: "consume_association",
        constants.WXF_CONSTANTS.Rule: "consume_rule",
        constants.WXF_CONSTANTS.RuleDelayed: "consume_rule_delayed",
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
                "Class %s does not implement any consumer method for WXF token %s"
                % (self.__class__.__name__, wxf_type)
            )
        return getattr(self, func)

    _LIST = WLSymbol("List")

    def consume_function(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *function*.

        Return a :class:`list` if the head is symbol `List`, otherwise returns the result of :func:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer.build_function`
        applied to the head and arguments.

        Usually custom parsing rules target Functions, but not List. To do so, it is recommended to override
        :func:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer.build_function`.
        """
        head = self.next_expression(tokens, **kwargs)
        args = tuple(
            self.next_expression(tokens, **kwargs) for i in range(current_token.length)
        )
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

    def consume_association(self, current_token, tokens, dict_class=immutabledict, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *association*.

        By default, return a :class:`dict` made from the rules.
        The named option `dict_class` can be set to any type in which case an instance of
        :class:`dict_class` is returned.
        """
        return dict_class(
            self.next_expression(tokens, **kwargs) for i in range(current_token.length)
        )

    def consume_rule(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *rule* as a tuple"""
        return (self.next_expression(tokens, **kwargs), self.next_expression(tokens, **kwargs))

    def consume_rule_delayed(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *rule* as a tuple"""
        return (self.next_expression(tokens, **kwargs), self.next_expression(tokens, **kwargs))

    BUILTIN_SYMBOL = {
        "True": True,
        "False": False,
        "Null": None,
        "Indeterminate": float("NaN"),
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
            raise WolframParserException("Invalid big integer value: %s" % current_token.data)

    BIGREAL_RE = re.compile(r"([^`]+)(`[0-9.]+){0,1}(\*\^){0,1}(-?[0-9]+){0,1}")

    def consume_bigreal(self, current_token, tokens, **kwargs):
        """Parse a WXF big real as a WXF serializable big real.

        There is not such thing as a big real, in Wolfram Language notation, in Python. This
        wrapper ensures round tripping of big reals without the need of `ToExpression`.
        Introducing `ToExpression` would imply to marshall the big real data to avoid malicious
        code from being introduced in place of an actual real.
        """

        match = self.BIGREAL_RE.match(current_token.data)

        if match:

            num, _, _, exp = match.groups()

            if exp:
                return decimal.Decimal("%se%s" % (num, exp))

            return decimal.Decimal(num)

        raise WolframParserException("Invalid big real value: %s" % current_token.data)

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
        return array_to_list(
            current_token.data,
            current_token.dimensions,
            constants.ARRAY_TYPES_FROM_WXF_TYPES[current_token.array_type],
        )

    def consume_packed_array(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *packed array*.

        This method return :class:`list`, and made the assumption that system is little endian.
        """
        return array_to_list(
            current_token.data,
            current_token.dimensions,
            constants.ARRAY_TYPES_FROM_WXF_TYPES[current_token.array_type],
        )


class WXFConsumerNumpy(WXFConsumer):
    """ A WXF consumer that maps WXF array types to NumPy arrays. """

    def consume_numeric_array(self, current_token, tokens, **kwargs):
        arr = numpy.frombuffer(
            current_token.data,
            dtype=WXFConsumerNumpy.WXF_TYPE_TO_DTYPE[current_token.array_type],
        )
        arr = numpy.reshape(arr, tuple(current_token.dimensions))
        return arr

    def consume_packed_array(self, current_token, tokens, **kwargs):
        arr = self.consume_numeric_array(current_token, tokens, **kwargs)
        return arr.view(numpy.PackedArray)

    # """Build a numpy array from a PackedArray."""
    # consume_packed_array = consume_packed_array
    # """Build a numpy array from a NumericArray."""
    # consume_numeric_array = consume_array

    WXF_TYPE_TO_DTYPE = {
        constants.ARRAY_TYPES.Integer8: "int8",
        constants.ARRAY_TYPES.Integer16: "int16",
        constants.ARRAY_TYPES.Integer32: "int32",
        constants.ARRAY_TYPES.Integer64: "int64",
        constants.ARRAY_TYPES.UnsignedInteger8: "uint8",
        constants.ARRAY_TYPES.UnsignedInteger16: "uint16",
        constants.ARRAY_TYPES.UnsignedInteger32: "uint32",
        constants.ARRAY_TYPES.UnsignedInteger64: "uint64",
        constants.ARRAY_TYPES.Real32: "float32",
        constants.ARRAY_TYPES.Real64: "float64",
        constants.ARRAY_TYPES.ComplexReal32: "complex64",
        constants.ARRAY_TYPES.ComplexReal64: "complex128",
    }
