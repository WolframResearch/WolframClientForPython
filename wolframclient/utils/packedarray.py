from __future__ import absolute_import, print_function, unicode_literals

import numpy as np


class PackedArray(np.ndarray):
    """Wrapper class on top of NymPy ndarray used to preserve packed arrays when round-tripping them."""
