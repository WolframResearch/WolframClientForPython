# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import os
import sys
import unittest

from wolframclient.cli.utils import SimpleCommand


class Command(SimpleCommand):
    """ Run test suites from the tests modules.
    A list of patterns can be provided to specify the tests to run.
    """

    def add_arguments(self, parser):
        parser.add_argument('args', nargs='*')

    def handle(self, *args):
        here = os.path.abspath(os.path.dirname(__file__))
        project_root = os.path.dirname(os.path.dirname(here))

        # args can be a list of patterns.
        if args:
            suite = unittest.TestSuite()
            for arg in args:
                suite.addTests(
                    unittest.defaultTestLoader.discover(
                        os.path.join(project_root, 'tests'),
                        pattern=arg,
                        top_level_dir=project_root))
        # take every single test from tests module.
        else:
            suite = unittest.defaultTestLoader.discover(
                os.path.join(project_root, 'tests'),
                pattern='*',
                top_level_dir=project_root)

        # verbosity > 1 print test name
        runner = unittest.TextTestRunner(verbosity=2)
        result = runner.run(suite)
        if not result.wasSuccessful():
            sys.exit(1)
