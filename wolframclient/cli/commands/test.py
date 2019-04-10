# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import sys
import unittest

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils import six
from wolframclient.utils.decorators import to_tuple
from wolframclient.utils.functional import map
from wolframclient.utils.importutils import module_path


@to_tuple
def dependencies():
    yield ("pytz", '2018.6')

    if not six.JYTHON:
        yield ("numpy", not six.PY2 and "1.15.3" or None)
        yield ("pillow", "5.3.0")
        yield ("requests", "2.20.0")
        yield ("oauthlib", "2.1.0")
        yield ("pyzmq", "17.1.2")
        yield ("pandas", "0.23.4")
        yield ('unittest-xml-reporting', None)
    if not six.PY2:
        yield ("aiohttp", "3.4.4")


class Command(SimpleCommand):
    """ Run test suites from the tests modules.
    A list of patterns can be provided to specify the tests to run.
    """

    modules = ['wolframclient.tests']

    dependencies = dependencies()

    def add_arguments(self, parser):
        parser.add_argument('-x', '--xml', dest='produce_xml', action='store_true',
                            help='produce xml reports from the test results')
        parser.add_argument('-d', '--xml-dir', dest='xml_output_dir',
                            help='produce an xml report in a specific directory.')
        parser.add_argument("-v", "--verbosity", type=int, choices=[0, 1, 2], default=2,
                            help="set output verbosity")
        parser.add_argument('args', nargs='*')

    def handle(self, *args, **opts):

        suite = unittest.TestSuite()
        for root in map(module_path, self.modules):
            for arg in args or ['*']:
                suite.addTests(
                    unittest.defaultTestLoader.discover(
                        root, pattern=arg, top_level_dir=root))

        # verbosity > 1 print test name
        verbosity = opts.get('verbosity')
        xml_path = opts.get('xml_output_dir')
        # if opts.get('produce_xml'):
        if xml_path is not None or opts.get('produce_xml'):
            import xmlrunner
            runner = xmlrunner.XMLTestRunner(output=xml_path or 'test-reports', verbosity=verbosity)
        else:
            runner = unittest.TextTestRunner(verbosity=verbosity)

        result = runner.run(suite)

        if not result.wasSuccessful():
            sys.exit(1)
