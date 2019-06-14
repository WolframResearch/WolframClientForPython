from __future__ import absolute_import, print_function, unicode_literals

import os

from wolframclient.serializers.wxfencoder.serializer import WXFExprSerializer
from wolframclient.serializers.wxfencoder.wxfencoder import WXFEncoder
from wolframclient.serializers.wxfencoder.wxfexpr import WXFExprFunction, WXFExprSymbol
from wolframclient.serializers.wxfencoder.wxfexprprovider import WXFExprProvider
from wolframclient.tests.configure import dir_test_data
from wolframclient.utils import six
from wolframclient.utils.tests import TestCase as BaseTestCase


class MyClass(object):
    def __init__(self, *values):
        self.values = values


class MyClass1(MyClass):
    def __init__(self, *values):
        super(MyClass1, self).__init__(*values)


class MyClass2(MyClass):
    def __init__(self, *values):
        super(MyClass2, self).__init__(*values)


class MyClassEncoder(WXFEncoder):
    def encode(self, o):
        if isinstance(o, MyClass):
            yield WXFExprFunction(len(o.values))
            yield WXFExprSymbol("Global`%s" % o.__class__.__name__)
            for sub in o.values:
                for wxfexpr in self.serialize(sub):
                    yield wxfexpr


class TestCase(BaseTestCase):
    def test_custom_encoder(self):
        """ test re-entrant calls """
        expr_provider = WXFExprProvider()
        expr_provider.add_encoder(MyClassEncoder())
        stream = six.BytesIO()
        serializer = WXFExprSerializer(stream, expr_provider=expr_provider)
        myclass2 = MyClass2(True, "foobar")
        myclass1 = MyClass1(1, None)
        myclass = MyClass1(1, 2, [myclass2, myclass1])
        o = ["foo", [1, {"k1": myclass, "k2": False}]]
        serializer.serialize(o)

        data_dir = dir_test_data()
        filepath = os.path.join(data_dir, "test.wxf")
        with open(filepath, "wb") as w_file:
            w_file.write(stream.getvalue())
        os.remove(filepath)
