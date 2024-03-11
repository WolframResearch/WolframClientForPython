from __future__ import absolute_import, print_function, unicode_literals

import zoneinfo

from wolframclient.utils.dispatch import Dispatch

encoder = Dispatch()


@encoder.dispatch(zoneinfo.ZoneInfo)
def encode_zoneinfo(serializer, o):
    return serializer.serialize_tzinfo(o)