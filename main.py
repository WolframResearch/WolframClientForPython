import time
import numpy
from wxfserializer.wxfexprprovidernumpy import WXFExprProviderNumPy
from wxfserializer.wxfexprprovider import WXFExprProvider
from wxfserializer.wxfdataconsumer import WXFDataConsumer, InMemoryWXFDataConsumer
from wxfserializer.serializer import WXFExprSerializer

def basicListOfString(lenght=10000):
    pyExpr = ['abcdefgh' for i in range(lenght)]
    start = time.perf_counter()
    expr_provider = WXFExprProvider()
    data_consumer = InMemoryWXFDataConsumer()
    serializer = WXFExprSerializer(expr_provider, data_consumer)
    serializer.serialize(pyExpr)
    stop = time.perf_counter()
    print('serialization of list of string took: ', stop - start)
    # print(data_consumer.data())
    with open('/tmp/pytest.wxf', 'wb') as output:
        output.write(data_consumer.data())


def basicNumPyArray(length=1e7):
    start = time.perf_counter()
    arr = numpy.empty(int(length), 'int16')
    arr.fill(int(5))
    expr_provider = WXFExprProviderNumPy()
    data_consumer = InMemoryWXFDataConsumer()
    serializer = WXFExprSerializer(expr_provider, data_consumer)
    serializer.serialize(arr)
    stop = time.perf_counter()
    with open('/tmp/numpytest.wxf', 'wb') as output:
        output.write(data_consumer.data())
    print('serialization of numpy array took: ', stop - start)


def mixNumPyAndBasicTypes():
    start = time.perf_counter()
    arr = numpy.empty(int(1e1), 'int16')
    arr.fill(-1)
    pyexpr = ["foo", 1, -512, arr]
    expr_provider = WXFExprProviderNumPy()
    data_consumer = InMemoryWXFDataConsumer()
    serializer = WXFExprSerializer(expr_provider, data_consumer)
    serializer.serialize(pyexpr)
    stop = time.perf_counter()
    with open('/tmp/numpytest2.wxf', 'wb') as output:
        output.write(data_consumer.data())
    print('serialization of mixed numpy array took: ', stop - start)


def main():
    basicListOfString()
    basicNumPyArray()
    mixNumPyAndBasicTypes()

    start = time.perf_counter()
    arr = numpy.empty(int(1e1), 'int16')
    arr.fill(-1)
    pyexpr = [1, ["foo"], [[]], [[], 1, []], '']
    expr_provider = WXFExprProviderNumPy()
    data_consumer = InMemoryWXFDataConsumer()
    serializer = WXFExprSerializer(expr_provider, data_consumer)
    serializer.serialize(pyexpr)
    stop = time.perf_counter()
    with open('/tmp/test.wxf', 'wb') as output:
        output.write(data_consumer.data())
    print('serialization of mixed numpy array took: ', stop - start)
main()
