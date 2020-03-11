# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from functools import partial

from wolframclient.serializers import export
from wolframclient.utils.api import numpy
from wolframclient.utils.pythonarray import PythonArray
from wolframclient.utils.tests import TestCase as BaseTestCase


class TestCase(BaseTestCase):

    export = partial(export, target_format="wl")

    def test_python_array(self):

        for array, numpy_type, wl_type in (([1, 2, 3], numpy.uint64, "Integer64"),):

            print(self.export(numpy.array(array, numpy_type)))
            print(self.export(PythonArray(array, wl_type)))
