# -*- coding: utf-8 -*-
import os
import unittest
import fnmatch
from wolframclient.utils.six import PY3
from . import test_cloud as m1
from . import test_kernel as m2
# Only Python 3.5 and above support async, so we need the test suite
# to avoid loading the module testing coroutines.
if PY3:
    from . import test_coroutine as m3
    test_modules = (m1, m2, m3)
else:
    test_modules = (m1, m2)

__all__ = ['load_tests']

def load_tests(loader, tests, pattern):
    suite = unittest.TestSuite()
    for module in test_modules:
        if fnmatch.fnmatch(os.path.basename(module.__file__), pattern):
            tests = loader.loadTestsFromModule(module)
            suite.addTests(tests)
    return suite

