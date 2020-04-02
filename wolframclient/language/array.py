from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.exception import WolframLanguageException
from wolframclient.serializers.wxfencoder import constants
from wolframclient.utils.encoding import concatenate_bytes

try:
    from collections.abc import Sequence
except ImportError:
    from collections import Sequence


class NumericArray(Sequence):
    def __init__(self, array, type, shape=None):

        self.array = array
        self.shape = shape or (len(array),)
        self.type = type
        self._valid_type_or_fail(type)
        self.struct = constants.STRUCT_MAPPING[type]

    def _valid_type_or_fail(self, type):
        if type not in constants.STRUCT_MAPPING:
            raise WolframLanguageException(
                "Type %s is not one of the supported array types: %s."
                % (type, ", ".join(constants.STRUCT_MAPPING.keys()))
            )

    def tobytes(self):
        return concatenate_bytes(self.struct.pack(el) for el in self.array)

    def __getitem__(self, k):
        return self.array[k]

    def __len__(self):
        return len(self.array)


class PackedArray(NumericArray):

    def _valid_type_or_fail(self, type):
        if type not in constants.VALID_PACKED_ARRAY_LABEL_TYPES:
            raise WolframLanguageException(
                "Type %s is not one of the supported packed array types: %s."
                % (type, ', '.join(constants.VALID_PACKED_ARRAY_LABEL_TYPES_TUPLE))
            )
