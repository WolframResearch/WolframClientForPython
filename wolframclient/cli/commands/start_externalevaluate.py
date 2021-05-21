from __future__ import absolute_import, print_function, unicode_literals

import os
import sys

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils.api import externalevaluate as ev
from wolframclient.utils.api import zmq


class Command(SimpleCommand):

    dependencies = ()

    def add_arguments(self, parser):
        parser.add_argument("--port", dest="port", default=None)
        parser.add_argument("--installpath", dest="installpath", default=None)

    def handle(self, port=None, installpath=None, **opts):

        if installpath:
            os.environ["WOLFRAM_INSTALLATION_DIRECTORY"] = installpath

        try:
            zmq.Context
        except ImportError as e:
            print(
                "Error importing zmq: %s. Please install zmq. https://zeromq.org/languages/python/"
                % e,
                file=sys.stderr,
            )
            sys.stderr.flush()
            sys.exit(1)

        ev.start_zmq_loop(port=port)
