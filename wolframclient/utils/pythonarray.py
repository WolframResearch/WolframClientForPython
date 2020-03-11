from __future__ import absolute_import, print_function, unicode_literals

from collections.abc import Sequence


class PythonArray(Sequence):
    def __init__(self, array, type):
        self.array = array
        self.type = type

    def __getitem__(self, k):
        return self.array[k]

    def __len__(self):
        return len(self.array)
