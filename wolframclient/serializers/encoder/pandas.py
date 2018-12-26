# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.dispatch import Dispatch
from wolframclient.serializers.base import FormatSerializer
from wolframclient.language import wl
from wolframclient.utils import six 

from itertools import starmap

from wolframclient.utils.api import pandas

encoder = Dispatch()

def safe_pandas_length(o):
    try:
        return o.shape[0]
    except (TypeError, IndexError):
        return

def encode_as_dataset(serializer, o, length):
    return serializer.serialize_function(
        serializer.serialize_symbol(b'Dataset'),
        (encode_as_association(serializer, o, length),),
    )

def encode_as_list(serializer, o, length):
    return serializer.serialize_iterable(starmap(lambda k,v : serializer.serialize_rule(k,v), encoded_kv_tuples(serializer, o)), length=length)

def encode_as_association(serializer, o, length):
    return serializer.serialize_association(
        encoded_kv_tuples(serializer, o), 
        length=length)

def encoded_kv_tuples(serializer, o):
    return ((serializer.encode(k),
            serializer.encode(v)) for k,v in o.items())


def encode_as_timeseries(serializer, o, length):
    return serializer.serialize_function(
        serializer.serialize_symbol(b'TimeSeries'),
        (serializer.serialize_iterable(
            (serializer.serialize_iterable(item, length=2) for item in 
                encoded_kv_tuples(serializer, o)), length=length),
        ),
    )

def encode_multiindex_as_assoc(serializer, o, length):
    return _iter_encode_multiindex_inner_assoc(serializer, o)

def encode_multiindex_as_dataset(serializer, o, length):
    return serializer.serialize_function(
        serializer.serialize_symbol(b'Dataset'),
            (_iter_encode_multiindex_inner_assoc(serializer, o),
            )
    )

def _iter_encode_multiindex_inner_assoc(serializer, o):
    expr_dict = {}
    for multikey, value in o.iteritems():
        cur_dict = expr_dict
        for key in multikey[:-1]:
            if key not in cur_dict:
                cur_dict[key] = {}
            cur_dict = cur_dict[key]
        cur_dict[multikey[-1]] = value
    return serializer.encode(expr_dict)

SERIES_PROPERTIES = {
    'pandas_series_head' : {'dataset', 'list', 'association'},
    'pandas_dataframe_head' : {'dataset', 'association'},
    'timeseries' : True,
}

SERIALIZE_FORMAT = {
    'dataset' : encode_as_dataset,
    'list' : encode_as_list,
    'association' : encode_as_association,
    'timeseries' : encode_as_timeseries,
    ('multiindex', 'association') : encode_multiindex_as_assoc,
    ('multiindex', 'dataset') : encode_multiindex_as_dataset,
}

def _get_encoder_from_index(index, use_ts, form):
    if use_ts and isinstance(index, pandas.DatetimeIndex):
        return 'timeseries'
    elif isinstance(index, pandas.MultiIndex):
        return ('multiindex', form or 'dataset')
    else:
        return form or 'association'

INVALID_PROPERTY_MSG = 'Invalid property %s, expecting %s'

def normalized_prop_timeseries(serializer):
    prop = serializer.get_property('timeseries', d=True)
    if not isinstance(prop, bool):
        raise ValueError("Invalid value for property 'timeseries'. Expecting a boolean, got %s." % prop)
    return prop

def normalized_prop_pandas_series_head(serializer):
    prop = serializer.get_property('pandas_series_head', d=None)
    if prop and prop not in SERIES_PROPERTIES['pandas_series_head']:
        raise ValueError("Invalid value for property 'pandas_series_head'. Expecting one of (%s), got %s." % (', '.join(SERIES_PROPERTIES['pandas_series_head']), prop))
    return prop


@encoder.dispatch((pandas.Series, pandas.DataFrame))
def encode_panda_series(serializer, o):
    use_ts = normalized_prop_timeseries(serializer)
    form = normalized_prop_pandas_series_head(serializer)
    format_tuple = _get_encoder_from_index(o.index, use_ts, form)
    return SERIALIZE_FORMAT[format_tuple](serializer, o, safe_pandas_length(o))
