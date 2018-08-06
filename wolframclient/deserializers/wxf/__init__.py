# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.deserializers.wxf.wxfconsumer import WXFConsumer, WXFConsumerNumpy
from wolframclient.deserializers.wxf.wxfparser import WXFParser
from wolframclient.exception import WolframParserException

__all__ = ['binary_deserialize']

def binary_deserialize(wxf_input, consumer=None, **kwargs):
    """Deserialize binary data, return a Python object.

    A stream of :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` is generated from the WXF input by a instance
    of :class:`~wolframclient.deserializers.wxf.wxfparser.WXFParser`.

    The consumer must be an instance of :class:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer`. If none is
    provided :class:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumerNumpy` is used.

    Named parameters are passed to the consumer. They can be any valid parameter of
    :meth:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer.next_expression`, namely:

    * `ordered_dict`: map WXF `Association` to :class:`~collections.OrderedDict` in place of a regular :class:`dict`.
    * `association`: map WXF `Association` to :class:`~wolframclient.utils.datastructures.Association` in place of a regular :class:`dict`.

    """
    parser = WXFParser(wxf_input)
    if consumer is None:
        consumer = WXFConsumerNumpy()
    else:
        if not isinstance(consumer, WXFConsumer):
            raise TypeError('Invalid consumer type.')
    tokens = parser.tokens()
    try:
        o = consumer.next_expression(tokens, **kwargs)
    except StopIteration:
        raise WolframParserException(
            'Input data does not represent a valid expression in WXF format. Expecting more input data.')
    if not parser.context.is_valid_final_state():
        raise WolframParserException(
            'Input data does not represent a valid expression in WXF format. Some expressions are imcomplete.')
    return o