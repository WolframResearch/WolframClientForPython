from __future__ import absolute_import, print_function, unicode_literals

import fnmatch
import os
import unittest

from wolframclient.utils import six

# The evaluation modules is only supported on python 3.5+, because of asyncio
# We need to prevent the test suite from loading this module containing coroutines.
if six.PY_35:
    from wolframclient.tests.evaluation import test_async_cloud as m4
    from wolframclient.tests.evaluation import test_cloud as m1
    from wolframclient.tests.evaluation import test_coroutine as m3
    from wolframclient.tests.evaluation import test_kernel as m2

    test_modules = (m1, m2, m3, m4)
else:
    test_modules = ()

__all__ = ["load_tests"]


def load_tests(loader, tests, pattern):
    suite = unittest.TestSuite()
    for module in test_modules:
        if fnmatch.fnmatch(os.path.basename(module.__file__), pattern):
            tests = loader.loadTestsFromModule(module)
            suite.addTests(tests)
    return suite
