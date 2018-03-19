# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.cli.utils import SimpleCommand
from wolframclient.language.expression import wl
from wolframclient.serializers import export, available_formats, DEFAULT_FORMAT
from wolframclient.utils.encoding import force_text

import decimal

class Command(SimpleCommand):

    def add_arguments(self, parser):
        parser.add_argument('--format', dest = 'format', choices=tuple(available_formats.keys()), default = DEFAULT_FORMAT)

    def handle(self, format):
        self.print(force_text(export(decimal.Decimal('1.2'), format = format)))