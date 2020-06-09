from __future__ import absolute_import, print_function, unicode_literals

import struct
from functools import reduce
from operator import mul

from wolframclient.exception import WolframLanguageException
from wolframclient.serializers.wxfencoder import constants
from wolframclient.utils.encoding import force_bytes

try:
    from collections.abc import Sequence
except ImportError:
    from collections import Sequence


def pack(format, *elements):
    return struct.pack(b"<%i%s" % (len(elements), force_bytes(format)), *elements)


class NumericArray(Sequence):
    def __init__(self, array, type, shape=None):

        self.array = array
        self.shape = shape or (len(array),)
        self.type = self._valid_type_or_fail(type)
        self.struct = constants.STRUCT_MAPPING[self.type]

    def _valid_type_or_fail(self, type):
        if type not in constants.STRUCT_MAPPING:
            raise WolframLanguageException(
                "Type %s is not one of the supported array types: %s."
                % (type, ", ".join(constants.STRUCT_MAPPING.keys()))
            )
        return type

    def tobytes(self):
        return pack(self.struct.format[1], *self.array)

    def __getitem__(self, k):
        return self.array[k]

    def __len__(self):
        return reduce(mul, self.shape, 1)


class PackedArray(NumericArray):
    def _valid_type_or_fail(self, type):
        if type not in constants.VALID_PACKED_ARRAY_LABEL_TYPES:
            raise WolframLanguageException(
                "Type %s is not one of the supported packed array types: %s."
                % (type, ", ".join(sorted(constants.VALID_PACKED_ARRAY_LABEL_TYPES)))
            )
        return type
