# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers import export
from wolframclient.utils.api import PIL
from wolframclient.utils.importutils import module_path
from wolframclient.utils.tests import TestCase as BaseTestCase


class TestCase(BaseTestCase):
    def test_pil(self):

        with PIL.open(
            module_path('wolframclient', 'tests', 'data', '5x2.png')) as image:

            self.assertEqual(
                export(image, target_format='wl'),
                b'ImportByteArray[ByteArray["iVBORw0KGgoAAAANSUhEUgAAAAUAAAACEAAAAADlkZXCAAAAH0lEQVR4nGP0+P39rf6+ky9/R7Aoen2+9shDWSRCHwCO7ws73c3PRQAAAABJRU5ErkJggg=="], "PNG"]'
            )
