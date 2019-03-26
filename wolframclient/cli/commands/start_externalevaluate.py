# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import os

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils.api import externalevaluate as ev


class Command(SimpleCommand):
    def add_arguments(self, parser):
        parser.add_argument('--port', dest='port', default=None)
        parser.add_argument('--installpath', dest='installpath', default=None)

    def handle(self, port=None, installpath=None, **opts):

        if installpath:
            os.environ['WOLFRAM_INSTALLATION_DIRECTORY'] = installpath

        ev.start_zmq_loop(port=port)
