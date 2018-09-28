# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.encoding import force_text
from wolframclient.utils.six import binary_type

import json
import sys

# Python 3.4 and 3.5 json loads only accept str.
if sys.version_info[0] == 3 and sys.version_info[1] <= 5:
    def loads(s, **kwargs):
        if isinstance(s, binary_type):
            s = force_text(s)
        return json.loads(s)
else:
    loads = json.loads