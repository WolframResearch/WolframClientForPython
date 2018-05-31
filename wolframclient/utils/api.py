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