# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.importutils import API

pytz = API(
    FixedOffset = 'pytz.FixedOffset',
    timezone    = 'pytz.timezone',
    utc         = 'pytz.utc',
    UnknownTimeZoneError = 'pytz.UnknownTimeZoneError'
)

json = API(
    dumps = 'json.dumps',
    dump  = 'json.dump',
    loads = 'json.loads',
    load  = 'json.load',
)

requests = API(
    get     = 'requests.get',
    post    = 'requests.post',
    request = 'requests.request',
)

oauth = API(
    Client = 'oauthlib.oauth1.Client',
    SIGNATURE_HMAC = 'oauthlib.oauth1.SIGNATURE_HMAC',
    SIGNATURE_TYPE_AUTH_HEADER = 'oauthlib.oauth1.SIGNATURE_TYPE_AUTH_HEADER',
)

pip = API(
    main = (
        'pip.main',
        'pip._internal.main'
    ),
    get_installed_distributions = (
        'pip.get_installed_distributions',
        'pip.utils.get_installed_distributions',
        'pip._internal.utils.misc.get_installed_distributions',
    ),
    virtualenv_no_global = (
        'pip.locations.virtualenv_no_global',
        'pip._internal.locations.virtualenv_no_global',
    )
)

zmq = API(
    Context = 'zmq.Context',
    PUSH    = 'zmq.PUSH',
    PULL    = 'zmq.PULL',
    PAIR    = 'zmq.PAIR',
    NOBLOCK = 'zmq.NOBLOCK',
    Again   = 'zmq.Again'
)

time = API(
    perf_counter = (
        'time.perf_counter',
        'time.time'
    ),
    sleep = 'time.sleep'
)
zmq = API(
    Context = 'zmq.Context',
    PUSH    = 'zmq.PUSH',
    PULL    = 'zmq.PULL',
    PAIR    = 'zmq.PAIR',
    NOBLOCK = 'zmq.NOBLOCK',
    Again   = 'zmq.Again'
)

time = API(
    perf_counter = (
        'time.perf_counter',
        'time.time'
    ),
    sleep = 'time.sleep'
)

urllib = API(
    urlparse = (
        'urllib.parse.urlparse',
        'urlparse.urlparse'
    ),
    parse_qs = (
        'urllib.parse.parse_qs',
        'urlparse.parse_qs',
    ),
    urlencode = (
        'urllib.parse.urlencode',
        'urllib.urlencode'
    ),
    quote_plus = (
        'urllib.parse.quote_plus',
        'urllib.quote_plus'
    )
)

PIL = API(
    Image = 'PIL.Image'
)