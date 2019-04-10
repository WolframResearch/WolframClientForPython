# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.deserializers.wxf.wxfconsumer import WXFConsumer
from wolframclient.deserializers.wxf.wxfparser import WXFParser
from wolframclient.exception import WolframParserException

__all__ = ['binary_deserialize']


def binary_deserialize(wxf_input, consumer=None, **kwargs):
    """Deserialize binary data and return a Python object.

    Serialize a Python object to WXF::

        >>> wxf = export({'key' : [1,2,3]}, target_format='wxf')

    Retrieve the input object::

        >>> binary_deserialize(wxf)
        {'key': [1, 2, 3]}

    A stream of :class:`~wolframclient.deserializers.wxf.wxfparser.WXFToken` is generated from the WXF input by a instance
    of :class:`~wolframclient.deserializers.wxf.wxfparser.WXFParser`.

    The consumer must be an instance of :class:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer`. If none is
    provided, :class:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer` is used. To enable NumPy array support,
    use :class:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumerNumpy`.

    Named parameters are passed to the consumer. They can be any valid parameter of
    :meth:`~wolframclient.deserializers.wxf.wxfconsumer.WXFConsumer.next_expression`, namely:

    * `dict_class`: map WXF `Association` to `dict_class` in place of a regular :class:`dict`

    """
    parser = WXFParser(wxf_input)
    if consumer is None:
        consumer = WXFConsumer()

    try:
        o = consumer.next_expression(parser.tokens(), **kwargs)
    except StopIteration:
        raise WolframParserException(
            'Input data does not represent a valid expression in WXF format. Expecting more input data.'
        )
    if not parser.context.is_valid_final_state():
        raise WolframParserException(
            'Input data does not represent a valid expression in WXF format. Some expressions are incomplete.'
        )
    return o
