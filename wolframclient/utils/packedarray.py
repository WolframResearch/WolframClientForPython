from __future__ import absolute_import, print_function, unicode_literals

import numpy


class PackedArray(numpy.ndarray):
    """ Wrapper class on top of NymPy ndarray used to preserve packed arrays when round-tripping them. """
