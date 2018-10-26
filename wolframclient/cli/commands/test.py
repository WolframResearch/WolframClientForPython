# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import sys
import unittest

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils import six
from wolframclient.utils.decorators import to_tuple
from wolframclient.utils.importutils import module_path


@to_tuple
def dependencies():
    yield ("pytz", '2018.6')

    if not six.JYTHON:
        yield ("numpy", "1.15.3")
        yield ("pillow", "5.3.0")
        yield ("requests", "2.20.0")
        yield ("oauthlib", "2.1.0")


class Command(SimpleCommand):
    """ Run test suites from the tests modules.
    A list of patterns can be provided to specify the tests to run.
    """

    modules = ['wolframclient.tests']

    dependencies = dependencies()

    def add_arguments(self, parser):
        parser.add_argument('args', nargs='*')

    def handle(self, *args):

        suite = unittest.TestSuite()
        for root in map(module_path, self.modules):
            for arg in args or ['*']:
                suite.addTests(
                    unittest.defaultTestLoader.discover(
                        root, pattern=arg, top_level_dir=root))

        # verbosity > 1 print test name
        runner = unittest.TextTestRunner(verbosity=2)
        result = runner.run(suite)
        if not result.wasSuccessful():
            sys.exit(1)
