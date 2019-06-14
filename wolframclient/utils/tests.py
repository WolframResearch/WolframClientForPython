from __future__ import absolute_import, print_function, unicode_literals

import unittest

from wolframclient.utils.importutils import module_path


class TestCase(unittest.TestCase):
    pass


def path_to_file_in_data_dir(*args):
    return module_path("wolframclient", "tests", "data", *args)
