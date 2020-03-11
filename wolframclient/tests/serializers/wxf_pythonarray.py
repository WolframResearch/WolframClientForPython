# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.array import PythonArray
from wolframclient.serializers import export
from wolframclient.utils.api import numpy
from wolframclient.utils.tests import TestCase as BaseTestCase


class TestCase(BaseTestCase):
    def test_python_array(self):

        for array, numpy_type, wl_type in (
            ([1, 2, 3], numpy.int8, "Integer8"),
            ([1, 2, 3], numpy.int32, "Integer32"),
            ([1, 2, 3], numpy.int64, "Integer64"),
            ([1.2, 2.3, 3], numpy.float32, "Real32"),
            ([1.2, 2.3, 3], numpy.float64, "Real64"),
        ):

            self.assertEqual(
                export(numpy.array(array, numpy_type), target_format="wxf"),
                export(PythonArray(array, wl_type), target_format="wxf"),
            )
