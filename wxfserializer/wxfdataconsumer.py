class WXFDataConsumer:
    '''
    Abstract class defining the functions used by `WXFExprSerializer` to write wxf bytes.
    '''
    def append(self, byte):
        raise NotImplementedError
    def extend(self, data):
        raise NotImplementedError

class InMemoryWXFDataConsumer(WXFDataConsumer):
    '''
    In memory implemetation.
    '''
    __slots__ = '_bytearray'
    def __init__(self):
        self._bytearray = bytearray()

    def append(self, b):
        self._bytearray.append(b)
        return self
    def extend(self, data):
        self._bytearray.extend(data)
        return self

    def data(self):
        return self._bytearray
