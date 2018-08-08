# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.exception import WolframParserException
from wolframclient.language.expression import WLFunction, WLSymbol
from wolframclient.serializers.wxfencoder import wxfexpr
from wolframclient.utils.api import numpy

__all__ = ['WXFConsumer', 'WXFConsumerNumpy']

class WXFConsumer(object):
    """Map WXF types to Python object generating functions.

    This class exposes a comprehensive list of methods consuming WXF types.
    Subclasses can override these members to implement custom parsing logic.

    Example: implement a consumer that parses Wolfram Language booleans :wl:`Null`
     as Python `None`::

        class ExampleConsumer(WXFConsumer):
            def consume_symbol(self, current_token, tokens, **kwargs):
                if current_token.data == 'Null':
                    return None
                else:
                    super().consume_symbol(current_token, tokens, **kwargs)

    Test the new consumer::

        >>> wxf = export({'a': 1, 'b': wl.Null}, target_format='wxf')
        >>> binary_deserialize(wxf, consumer=ExampleConsumer())
        {'a': 1, 'b': None}

    Compare with default result::

        >>> binary_deserialize(wxf)
        {'a': 1, 'b': Null}

    Once initialized, the entry point of a consumer is the method
    :func:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer.next_expression`
    that takes a token generator and return a Python object. This method is particularly
    useful when building nested expressions, e.g:
    :func:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer.consume_function`,
    :func:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer.consume_association`, etc,
    in order to fetch sub-expressions.
    """

    _mapping = {
        wxfexpr.WXF_CONSTANTS.Function: 'consume_function',
        wxfexpr.WXF_CONSTANTS.Symbol: 'consume_symbol',
        wxfexpr.WXF_CONSTANTS.String: 'consume_string',
        wxfexpr.WXF_CONSTANTS.BinaryString: 'consume_binary_string',
        wxfexpr.WXF_CONSTANTS.Integer8: 'consume_integer8',
        wxfexpr.WXF_CONSTANTS.Integer16: 'consume_integer16',
        wxfexpr.WXF_CONSTANTS.Integer32: 'consume_integer32',
        wxfexpr.WXF_CONSTANTS.Integer64: 'consume_integer64',
        wxfexpr.WXF_CONSTANTS.Real64: 'consume_real64',
        wxfexpr.WXF_CONSTANTS.BigInteger: 'consume_bigint',
        wxfexpr.WXF_CONSTANTS.BigReal: 'consume_bigreal',
        wxfexpr.WXF_CONSTANTS.PackedArray: 'consume_packed_array',
        wxfexpr.WXF_CONSTANTS.RawArray: 'consume_raw_array',
        wxfexpr.WXF_CONSTANTS.Association: 'consume_association',
        wxfexpr.WXF_CONSTANTS.Rule: 'consume_rule',
        wxfexpr.WXF_CONSTANTS.RuleDelayed: 'consume_rule_delayed'
    }

    def next_expression(self, tokens, **kwargs):
        """Deserialize the next expression starting at the next token yield by `tokens`."""
        token = next(tokens)
        consumer = self._consumer_from_type(token.wxf_type)
        return consumer(token, tokens, **kwargs)

    def _consumer_from_type(self, wxf_type):
        try:
            func = WXFConsumer._mapping[wxf_type]
        except KeyError:
            raise WolframParserException('Class %s does not implement any consumer method for WXF token %s' %s (
                cls.__name__, 
                token
            ))
        return getattr(self, func)

    _LIST = WLSymbol('List')
    
    def consume_function(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *function*.

        Return a :class:`list` if the head is symbol `List`, otherwise return a :class:`~wolframclient.language.expression.WLFunction`
        """
        head = self.next_expression(tokens, **kwargs)
        args = []
        for i in range(current_token.length):
            args.append(self.next_expression(tokens, **kwargs))
        if head == self._LIST:
            return args
        else:
            return WLFunction(head, *args)

    def consume_association(self, current_token, tokens, dict_class = dict, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *association*.

        By default, return a :class:`dict` made from the rules.
        The named option `dict_class` can be set to any type in which case an instance of
        :class:`dict_class` is returned.
        """
        return dict_class(
            self.next_expression(tokens, **kwargs)
            for i in range(current_token.length)
        )

    def consume_rule(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *rule* as a tuple"""
        return (self.next_expression(tokens, **kwargs), self.next_expression(tokens, **kwargs))

    def consume_rule_delayed(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *rule* as a tuple"""
        return (self.next_expression(tokens, **kwargs), self.next_expression(tokens, **kwargs))

    def consume_symbol(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *symbol* as a :class:`~wolframclient.language.expression.WLSymbol`"""
        return WLSymbol(current_token.data)

    def consume_bigint(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *big integer* as a :class:`int`."""
        try:
            return int(current_token.data)
        except ValueError:
            raise WolframParserException(
                'Invalid big integer value: %s' % current_token.data)

    def consume_bigreal(self, current_token, tokens, **kwargs):
        """Parse a WXF big real as a WXF serializable big real.

        There is not such thing as a big real, in Wolfram Language notation, in Python. This
        wrapper ensures round tripping of big reals without the need of `ToExpression`.
        Introducing `ToExpression` would imply to marshall the big real data to avoid malicious
        code from being introduced in place of an actual real.
        """
        return wxfexpr.WXFExprBigReal(current_token.data)

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

    def consume_raw_array(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *raw array*.

        This method must be implemented by subclasses."""
        raise NotImplementedError(
            'Method consume_raw_array is not implemented by %s.' % self.__class__.__name__)

    def consume_packed_array(self, current_token, tokens, **kwargs):
        """Consume a :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` of type *packed array*.

        This method must be implemented by subclasses."""
        raise NotImplementedError(
            'Method consume_packed_array is not implemented by %s.' % self.__class__.__name__)

class WXFConsumerNumpy(WXFConsumer):

    def consume_array(self, current_token, tokens, **kwargs):
        arr=numpy.fromstring(current_token.data, dtype=WXFConsumerNumpy.WXF_TYPE_TO_DTYPE[current_token.array_type])
        numpy.reshape(arr, tuple(current_token.dimensions))
        return arr
    """Build a numpy array from a PackedArray."""
    consume_packed_array = consume_array

    """Build a numpy array from a RawArray."""
    consume_raw_array = consume_array

    WXF_TYPE_TO_DTYPE = {
        wxfexpr.ARRAY_TYPES.Integer8: 'int8',
        wxfexpr.ARRAY_TYPES.Integer16: 'int16',
        wxfexpr.ARRAY_TYPES.Integer32: 'int32',
        wxfexpr.ARRAY_TYPES.Integer64: 'int64',
        wxfexpr.ARRAY_TYPES.UnsignedInteger8: 'uint8',
        wxfexpr.ARRAY_TYPES.UnsignedInteger16: 'uint16',
        wxfexpr.ARRAY_TYPES.UnsignedInteger32: 'uint32',
        wxfexpr.ARRAY_TYPES.UnsignedInteger64: 'uint64',
        wxfexpr.ARRAY_TYPES.Real32: 'float32',
        wxfexpr.ARRAY_TYPES.Real64: 'float64',
        wxfexpr.ARRAY_TYPES.ComplexReal32: 'complex64',
        wxfexpr.ARRAY_TYPES.ComplexReal64: 'complex128',
    }
