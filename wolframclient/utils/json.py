from __future__ import absolute_import, print_function, unicode_literals

import sys
from json import loads as json_loads

from wolframclient.utils import six
from wolframclient.utils.encoding import force_text

# Python 3.4 and 3.5 json loads only accept str.
if sys.version_info[0] == 3 and sys.version_info[1] <= 5:

    def loads(s, **kwargs):
        if isinstance(s, six.binary_type):
            s = force_text(s)
        return json_loads(s)


else:
    loads = json_loads
