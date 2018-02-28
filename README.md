# Python serialization to WXF

This library is intended to provide a way to serialize python expression to WXF string of bytes. The library was designed to be extensible so that any arbitrary Python object can be serialized with the addition of custom encoder(s). The code was tested with two interpreters: Python 2.7 and Python 3.6.4 on MacOS.

# API

## `WXFSerializer`

This is the central class that iterate on an expression provider and produces bytes sent to a data consumer. It is in charge of enforcing the consistency of the output data, if an invalid sequence of expression is received it must fail.

## `WXFExprProvider`

A generator of instances of `WXFExpr*` classes (i.e: sub-classes of the internal class `_WXFExpr`) that we will call just `WXFExpr` for convenience purpose. This class relies on an encoder to produce `WXFExpr` and supports a `default` function applied when no encoding was found for a given python expression. Default is typically `str`.

## `WXFEncoder`

This class is a generator of `WXFExpr` which can also be chained. Define transformation rules from a python expression to a (list of) `WXFExpr`. During initialization of the encoder it is possible to provide a fallback encoder. If no rule matches a given expression it is possible to delegate to the fallback encoder using `self.fallback(python_expr)`. If no encoder can deal with a given expression a `TypeError` is eventually raised.

The default encoder is `DefaultWXFEncoder` and is fairly restricted on purpose. For example it does not support iterator. It only deals with native type that maps directly to a given `WXFExpr` (basically JSON) and for which there shouldn't be any conflict with what the user wants. As such, is should always be used as the inner most fallback encoder.

On example of the extension mechanism is provided in `NumPyWXFEncoder` which add support for numpy `array` and `ndarray`. Both are encoded as `PackedArray` and eventually `RawArray`.

## `WXFDataConsumer`

Data consumer is a simple class that implement both `append` and `extend`, typically a wrapper around a `bytearray`, that the serializer use to store output binary data.

## Example:

Initializing an expression provider using the default encoder:
```
expr_provider = WXFExprProvider()
```
Create an in memory data comsumer:
```
data_consumer = InMemoryWXFDataConsumer()
```
Finally initialize a serializer with both:
```
serializer = WXFExprSerializer(expr_provider, data_consumer)
```
Serialize a given python expression and retrieve the data:
```
serializer.serialize(
    {
        'list' : [1,2,3],
        'int' : 123
    }
)
wxf_bytes = data_consumer.data()
```

One can then write those bytes to a file:
```
with open("/path/to/file", 'wb') as w_file:
    w_file.write(wxf_bytes)
```
So that it is then possible to import them in the WolframLanguage:
```
In[1]:= Import["/path/to/file", "WXF"]
Out[1]= <|list->{1,2,3},int->123|>
```

# Test suite

The test suite was executed on both supported version of python.
It is executed from the root directory with:
```
python -m unittest discover
```
