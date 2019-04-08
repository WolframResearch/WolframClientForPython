# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from collections import OrderedDict
from itertools import starmap

from wolframclient.utils.api import pandas
from wolframclient.utils.dispatch import Dispatch

encoder = Dispatch()


def safe_pandas_length(o):
    """ Return the length of a pandas Series and DataFrame as expected for WL serialization.

    - The length of a Series is the only value of the tuple `shape`.
    - The length of a dataframe is the number of columns. It's the second value of `shape`.

    This function is safe, when the shape does not have the expected number of elements, it fails silently and
    returns `None`, the object is later traversed to find out how many elements it contains.
    """
    try:
        return o.shape[-1]
    except (TypeError, IndexError):
        return


def encode_as_dataset(serializer, o, length):
    return serializer.serialize_function(
        serializer.serialize_symbol(b'Dataset'),
        (encode_as_association(serializer, o, length), ),
    )


def encode_as_list(serializer, o, length):
    return serializer.serialize_iterable(
        starmap(lambda k, v: serializer.serialize_rule(k, v),
                encoded_kv_tuples(serializer, o)),
        length=length)


def encode_as_association(serializer, o, length):
    return serializer.serialize_association(
        encoded_kv_tuples(serializer, o), length=length)


def encoded_kv_tuples(serializer, o):
    return ((serializer.encode(k), serializer.encode(v)) for k, v in o.items())


def encode_as_timeseries(serializer, o, length):
    return serializer.serialize_function(
        serializer.serialize_symbol(b'TimeSeries'),
        (serializer.serialize_iterable(
            (serializer.serialize_iterable(item, length=2)
             for item in encoded_kv_tuples(serializer, o)),
            length=length), ),
    )


def _distribute_multikey(o):
    expr_dict = OrderedDict()
    for multikey, value in o.iteritems():
        cur_dict = expr_dict
        for key in multikey[:-1]:
            if key not in cur_dict:
                cur_dict[key] = OrderedDict()
            cur_dict = cur_dict[key]
        cur_dict[multikey[-1]] = value
    return expr_dict


def encode_multiindex_as_assoc(serializer, o, length):
    return serializer.encode(_distribute_multikey(o))


def encode_multiindex_as_dataset(serializer, o, length):
    return serializer.serialize_function(
        serializer.serialize_symbol(b'Dataset'),
        (serializer.encode(_distribute_multikey(o)), ))


PANDAS_PROPERTIES = {
    'pandas_series_head': {'dataset', 'list', 'association'},
    'pandas_dataframe_head': {'dataset', 'association'},
    'timeseries': True,
}

ENCODERS = {
    'default': {
        'dataset': encode_as_dataset,
        'list': encode_as_list,
        'association': encode_as_association,
    },
    'datetimeindex': encode_as_timeseries,
    'multiindex': {
        'association': encode_multiindex_as_assoc,
        'list': encode_multiindex_as_assoc,
        'dataset': encode_multiindex_as_dataset
    },
}


def get_series_encoder_from_index(index, use_ts, form):
    if use_ts and isinstance(index, pandas.DatetimeIndex):
        return ENCODERS['datetimeindex']
    elif isinstance(index, pandas.MultiIndex):
        return ENCODERS['multiindex'][form or 'dataset']
    else:
        return ENCODERS['default'][form or 'association']


INVALID_PROPERTY_MSG = 'Invalid property %s, expecting %s'


def normalized_prop_timeseries(serializer):
    prop = serializer.get_property('timeseries', d=True)
    if not isinstance(prop, bool):
        raise ValueError(
            "Invalid value for property 'timeseries'. Expecting a boolean, got %s."
            % prop)
    return prop


def normalized_prop_pandas_series_head(serializer):
    """ Check property `pandas_series_head` only if specified (not None). """
    prop = serializer.get_property('pandas_series_head', d=None)
    if prop and prop not in PANDAS_PROPERTIES['pandas_series_head']:
        raise ValueError(
            "Invalid value for property 'pandas_series_head'. Expecting one of (%s), got %s."
            % (', '.join(PANDAS_PROPERTIES['pandas_series_head']), prop))
    return prop


@encoder.dispatch(pandas.Series)
def encode_panda_series(serializer, o):
    use_ts = normalized_prop_timeseries(serializer)
    form = normalized_prop_pandas_series_head(serializer)
    encoder = get_series_encoder_from_index(o.index, use_ts, form)
    return encoder(serializer, o, safe_pandas_length(o))


def encode_dataframe_as_assoc(serializer, o, length):
    use_ts = normalized_prop_timeseries(serializer)
    return serializer.serialize_association(
        ((serializer.encode(k),
          get_series_encoder_from_index(v.index, use_ts, 'association')(
              serializer, v, safe_pandas_length(v))) for k, v in o.T.items()),
        length=length)


def encode_dataframe_as_dataset(serializer, o, length):
    return serializer.serialize_function(
        serializer.serialize_symbol(b'Dataset'),
        (encode_dataframe_as_assoc(serializer, o, length), ))


@encoder.dispatch(pandas.DataFrame)
def encoder_panda_dataframe(serializer, o):
    head = serializer.get_property('pandas_dataframe_head', d=None)
    if head is None or head == 'dataset':
        return encode_dataframe_as_dataset(serializer, o,
                                           safe_pandas_length(o.index))
    elif head in PANDAS_PROPERTIES['pandas_dataframe_head']:
        return encode_dataframe_as_assoc(serializer, o,
                                         safe_pandas_length(o.index))
    else:
        raise ValueError(
            "Invalid value for property 'pandas_dataframe_head'. Expecting one of (%s), got %s."
            % (', '.join(PANDAS_PROPERTIES['pandas_dataframe_head']), head))
