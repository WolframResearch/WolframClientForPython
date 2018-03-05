# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from tests.test_wxf_serialization import init

from wxfserializer.serializer import WXFExprSerializer
from wxfserializer.utils import six
from wxfserializer.wxfdataconsumer import InMemoryWXFDataConsumer
from wxfserializer.wxfencoder import DefaultWXFEncoder, WXFEncoder
from wxfserializer.wxfexpr import WXFExprFunction, WXFExprSymbol
from wxfserializer.wxfexprprovider import WXFExprProvider

import unittest

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
            yield WXFExprSymbol('Global`%s' % o.__class__.__name__)
            for sub in o.values:
                for wxfexpr in self.serialize(sub):
                    yield wxfexpr

class TestEncoder(unittest.TestCase):
    def test_custom_encoder(self):
        ''' test re-entrant calls '''
        expr_provider = WXFExprProvider()
        expr_provider.add_encoder(MyClassEncoder())
        data_consumer = InMemoryWXFDataConsumer()
        serializer = WXFExprSerializer(expr_provider, data_consumer)
        myclass2 = MyClass2(True, 'foobar')
        myclass1 = MyClass1(1, None)
        myclass = MyClass1(1, 2, [myclass2, myclass1])
        o = ['foo', [1, {'k1': myclass, 'k2': False}]]
        serializer.serialize(o)
        with open('/tmp/test.wxf', 'wb') as w_file:
            w_file.write(data_consumer.data())