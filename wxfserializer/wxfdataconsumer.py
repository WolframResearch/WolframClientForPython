"""

"""
class WXFDataConsumer:
    def append(self, byte):
        raise NotImplementedError
    def extend(self, data):
        raise NotImplementedError

class InMemoryWXFDataConsumer(WXFDataConsumer):
    __slots__ = '_bytearray'
    def __init__(self):
        self._bytearray = bytearray(b'8:')

    def append(self, b):
        self._bytearray.append(b)
    def extend(self, data):
        self._bytearray.extend(data)

    def data(self):
        return self._bytearray
