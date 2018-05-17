# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.cli.utils import discover_with_convention, SimpleCommand
from wolframclient.utils import six
from wolframclient.utils.importutils import import_string
from wolframclient.utils.require import require_module

import sys
import unittest

class Command(SimpleCommand):

    modules = ['wolframclient.tests']
    class_name = 'TestCase'

    if six.JYTHON:
        dependencies = [
            ("pytz",  None),
        ]
    else:
        dependencies = [
            ("pytz",  None),
            ("numpy", None),
        ]

    @property
    def available_test_cases(self):
        try:
            return self._test_cases
        except AttributeError:
            self._test_cases = discover_with_convention(
                self.modules,
                self.class_name,
                walk = True
            )
            return self._test_cases

    def add_arguments(self, parser):
        parser.add_argument('args', nargs='*')

    def import_test_case(self, name):

        if not name in self.available_test_cases:

            #we need to validate names manually, I was not able to use .add_argument(choices = ...)

            self.print('Available test suites:')
            for t in sorted(self.available_test_cases.keys()):
                self.print(' -', t)

            self.print()
            self.print('%s is not a defined test suite. Create a TestCase in %s' % (
                name,
                " or ".join('%s.%s.%s' % (m, name, self.class_name) for m in self.modules)
            ))

            sys.exit(1)

        return import_string(self.available_test_cases[name])

    def handle(self, *args):

        require_module(*self.dependencies)

        suite = unittest.TestSuite()

        for arg in (args or self.available_test_cases.keys()):
            test_case = self.import_test_case(arg)

            for func in test_case.discover_tests():
                suite.addTest(test_case(func))

        runner = unittest.TextTestRunner()
        result = runner.run(suite)
        if not result.wasSuccessful():
            sys.exit(1)