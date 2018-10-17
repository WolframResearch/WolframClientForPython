# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import os
import sys
import unittest

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils.importutils import module_path

class Command(SimpleCommand):
    """ Run test suites from the tests modules.
    A list of patterns can be provided to specify the tests to run.
    """

    modules = ['wolframclient.tests']

    def add_arguments(self, parser):
        parser.add_argument('args', nargs='*')

    def handle(self, *args):

        suite = unittest.TestSuite()
        for root in map(module_path, self.modules):
            for arg in args or ['*']:
                suite.addTests(
                    unittest.defaultTestLoader.discover(
                        root,
                        pattern=arg,
                        top_level_dir=root))

        # verbosity > 1 print test name
        runner = unittest.TextTestRunner(verbosity=2)
        result = runner.run(suite)
        if not result.wasSuccessful():
            sys.exit(1)
