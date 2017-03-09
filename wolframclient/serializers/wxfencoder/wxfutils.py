# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import re

INT8_MAX = 1 << 7
INT8_MIN = -(1 << 7)
INT16_MAX = 1 << 15
INT16_MIN = -(1 << 15)
INT32_MAX = 1 << 31
INT32_MIN = -(1 << 31)
INT64_MAX = 1 << 63
INT64_MIN = -(1 << 63)

class VersionParser(object):
    ''' Parse a string version of the form {{major}}.{{minor}}.* to extract the major and minor values '''

    def __init__(self, version):
        if not re.match(r'\d[.]\d+[.].*', version):
            raise ValueError("Version string is not valid.")
        values = version.split('.')
        self.version = version
        self.major = int(values[0])
        self.minor = int(values[1])