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
    dumps           = 'json.dumps',
    dump            = 'json.dump',
    loads           = 'json.loads',
    load            = 'json.load',
    JSONDecodeError = 'json.decoder.JSONDecodeError'
)

zlib = API(
    compressobj='zlib.compressobj',
    decompressobj='zlib.decompressobj'
)

os = API(
    X_OK        = 'os.X_OK',
    access      = 'os.access',
    expandvars  = 'os.path.expandvars',
    expanduser  = 'os.path.expanduser',
    dirname     = 'os.path.dirname',
    path_join   = 'os.path.join',
    isfile      = 'os.path.isfile'
)

futures = API(
    ThreadPoolExecutor='concurrent.futures.ThreadPoolExecutor'
)

requests = API(
    get     = 'requests.get',
    post    = 'requests.post',
    request = 'requests.request',
    Request = 'requests.Request',
    Session = 'requests.Session'
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
    SUB     = 'zmq.SUB',
    SUBSCRIBE = 'zmq.SUBSCRIBE',
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
    ),
    quote=(
        'urllib.parse.quote',
        'urllib.quote'
    )
)

atexit = API(
    register = 'atexit.register',
    unregister = ('atexit.unregister',)
)

numpy = API(
    ndarray = 'numpy.ndarray',
    int8 = 'numpy.int8',
    int16 = 'numpy.int16',
    int32 = 'numpy.int32',
    int64 = 'numpy.int64',
    uint8 = 'numpy.uint8',
    uint16 = 'numpy.uint16',
    uint32 = 'numpy.uint32',
    uint64 = 'numpy.uint64',
    float32 = 'numpy.float32',
    float64 = 'numpy.float64',
    complex64 = 'numpy.complex64',
    complex128 = 'numpy.complex128',
    dtype = 'numpy.dtype',
    fromstring = 'numpy.fromstring',
    reshape = 'numpy.reshape'
)
