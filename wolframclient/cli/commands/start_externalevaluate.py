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
        parser.add_argument("--kernelversion", dest="kernelversion", default=None)

    def handle(self, port=None, installpath=None, kernelversion=None, **opts):

        for key, value in (
            ("WOLFRAM_INSTALLATION_DIRECTORY", installpath),
            ("WOLFRAM_KERNEL_VERSION", kernelversion),
        ):
            if value:
                os.environ[key] = value

        try:
            zmq.Context
        except ImportError as e:
            print(
                'Error importing zmq: %s. Please install zmq by running:\nExternalEvaluate[{"Shell", "Target" :> $SystemShell}, "%s" -> {"-m", "pip", "install", "pyzmq", "--user", "--no-input"}]'
                % (e, sys.executable),
                file=sys.stderr,
            )
            sys.stderr.flush()
            sys.exit(1)

        ev.start_zmq_loop(port=port)
