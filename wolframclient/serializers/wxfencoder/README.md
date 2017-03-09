# Python serialization to WXF

This library is intended to provide a way to serialize python expression to WXF string of bytes. The library was designed to be extensible so that any arbitrary Python object can be serialized with the addition of custom encoder(s). The code was tested with two interpreters: Python 2.7 and Python 3.6.4 on MacOS.

# API

## `WXFSerializer`

This is the central class that iterate on an expression provider and produces bytes sent to a data consumer. It is in charge of enforcing the consistency of the output data, if an invalid sequence of expression is received it must fail.

## `WXFExprProvider`

A generator of instances of `WXFExpr*` classes (i.e: sub-classes of the internal class `_WXFExpr`) that we will call just `WXFExpr` for convenience purpose. This class relies on encoder(s) to generate `WXFExpr`, one provider can have more than one encoder, each dealing with a subset of classes. It also supports a `default` function applied when no encoding was found for a given python expression, typically `str`.

## `WXFEncoder`

This class is a generator of `WXFExpr` which can also be chained. It defines transformation rules from a python expression to a (stream of) `WXFExpr`. If no rule matches a given expression it should return without yielding to signal that the object should be passed to the next encoder. If no encoder can deal with a given expression a `TypeError` is eventually raised.

The default encoder is `DefaultWXFEncoder` and support a fairly limited number of type, on purpose. For example it does not support iterators. It only deals with native types that maps directly to a given `WXFExpr` (basically JSON) and for which there shouldn't be any conflict in most use cases. As such, is should always be used as the first encoder.

One example of the extension mechanism is provided in `NumPyWXFEncoder` which add support for numpy `array` and `ndarray`. Both are encoded as `PackedArray` and eventually `RawArray`.

### Extending encoding

Here is an example of a user defined encoder `MyClassEncoder` dealing with a custom class `MyClass`. The class is a wrapper around a tuple, and the encoder is encoding instances as a Wolfram Language function `MyClass[values]`.

```
class MyClass(object):
    def __init__(self, *values):
        self.values = values

class MyClassEncoder(WXFEncoder):
    def encode(self, o):
        if isinstance(o, MyClass):
            yield WXFExprFunction(len(o.values))
            yield WXFExprSymbol('Global`%s' % o.__class__.__name__)
            for sub in o.values:
                for wxfexpr in self.serialize(sub):
                    yield wxfexpr
```

Initialize the serializer with the new encoder:
```
expr_provider = WXFExprProvider()
expr_provider.add_encoder(MyClassEncoder())
data_consumer = InMemoryDataConsumer()
serializer = WXFExprSerializer(expr_provider, data_consumer)
```

Serialize an expression an write the bytes to a file `/tmp/test.wxf`:
```
py_expr = MyClass('foo', MyClass('bar'))
serializer.serialize(py_expr)
with open('/tmp/test.wxf', 'wb') as w_file:
    w_file.write(data_consumer.data())
```

Once deserialized by a kernel using `Import["/tmp/test.wxf"]` the output expression is `MyClass["foo", MyClass["bar"]]`.

## `DataConsumer`

Data consumer is a simple class that implement both `append` and `extend`, typically a wrapper around a `bytearray`, that the serializer uses to store output binary data.

## Example:

Initializing an expression provider using the default encoder:
```
expr_provider = WXFExprProvider()
```
Create an in memory data comsumer:
```
data_consumer = InMemoryDataConsumer()
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
