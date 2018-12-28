# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers import export
from wolframclient.utils.api import PIL, numpy
from wolframclient.utils.tests import TestCase as BaseTestCase
from wolframclient.utils.tests import path_to_file_in_data_dir


class TestCase(BaseTestCase):
    def test_png_mode_I(self):

        with PIL.open(path_to_file_in_data_dir('5x2.png')) as image:

            self.assertEqual(
                export(image, target_format='wl'),
                b'ImportByteArray[ByteArray["iVBORw0KGgoAAAANSUhEUgAAAAUAAAACEAAAAADlkZXCAAAAH0lEQVR4nGP0+P39rf6+ky9/R7Aoen2+9shDWSRCHwCO7ws73c3PRQAAAABJRU5ErkJggg=="], "PNG"]'
            )

    def test_mode_L(self):
        a = numpy.arange(10).reshape((2, 5))
        img = PIL.fromarray(a, mode='L')
        out = export(img, target_format='wl')
        self.assertTrue(
            out ==
            b'Image[BinaryDeserialize[ByteArray["ODrCEAICBQAAAAAAAAAAAQA="]], "Byte", Rule[ColorSpace, "Grayscale"], Rule[Interleaving, True]]'
            or out ==
            b'Image[BinaryDeserialize[ByteArray["ODrCEAICBQAAAAAAAAAAAQA="]], "Byte", Rule[Interleaving, True], Rule[ColorSpace, "Grayscale"]]'
        )

    def test_bool_img(self):
        a = numpy.array([[1, 0], [0, 1]], dtype='bool')
        img = PIL.fromarray(a)
        out = export(img, target_format='wl')
        self.assertTrue(
            out ==
            b'Image[BinaryDeserialize[ByteArray["ODrCEAICAgEAAAA="]], "Bit", Rule[ColorSpace, Automatic], Rule[Interleaving, True]]'
            or out ==
            b'Image[BinaryDeserialize[ByteArray["ODrCEAICAgEAAAA="]], "Bit", Rule[Interleaving, True], Rule[ColorSpace, Automatic]]'
        )
