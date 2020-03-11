from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers.wxfencoder import constants
from wolframclient.utils import six

try:
    from collections.abc import Sequence
except ImportError:
    from collections import Sequence


class PythonArray(Sequence):
    def __init__(self, array, type, shape=None):

        self.array = array
        self.shape = shape or (len(array),)

        if isinstance(type, six.string_types):
            self.type = constants.ARRAY_TYPES[type]
        else:
            self.type = type

    def tobytes(self):
        raise NotImplementedError("aaa")

    def __getitem__(self, k):
        return self.array[k]

    def __len__(self):
        return len(self.array)
