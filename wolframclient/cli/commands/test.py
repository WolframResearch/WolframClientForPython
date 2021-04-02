from __future__ import absolute_import, print_function, unicode_literals

import sys
import unittest

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils.functional import map
from wolframclient.utils.importutils import module_path


class Command(SimpleCommand):
    """ Run test suites from the tests modules.
    A list of patterns can be provided to specify the tests to run.
    """

    modules = ["wolframclient.tests"]

    def add_arguments(self, parser):
        parser.add_argument(
            "-x",
            "--xml",
            dest="produce_xml",
            action="store_true",
            help="produce xml reports from the test results",
        )
        parser.add_argument(
            "-d",
            "--xml-dir",
            dest="xml_output_dir",
            help="produce an xml report in a specific directory.",
        )
        parser.add_argument(
            "-v",
            "--verbosity",
            type=int,
            choices=[0, 1, 2],
            default=2,
            help="set output verbosity",
        )
        parser.add_argument("args", nargs="*")

    def handle(self, *args, **opts):

        suite = unittest.TestSuite()
        for root in map(module_path, self.modules):
            for arg in args or ["*"]:
                suite.addTests(
                    unittest.defaultTestLoader.discover(root, pattern=arg, top_level_dir=root)
                )

        # verbosity > 1 print test name
        verbosity = opts.get("verbosity")
        # for consistency with parser, set default to 2. This is only possible when calling test from setup.py
        if verbosity is None:
            verbosity = 2
        xml_path = opts.get("xml_output_dir")
        # if opts.get('produce_xml'):
        if xml_path is not None or opts.get("produce_xml"):
            import xmlrunner

            runner = xmlrunner.XMLTestRunner(
                output=xml_path or "test-reports", verbosity=verbosity
            )
        else:
            runner = unittest.TextTestRunner(verbosity=verbosity)

        result = runner.run(suite)

        if not result.wasSuccessful():
            sys.exit(1)
