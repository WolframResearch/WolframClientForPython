# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.importutils import API

pytz = API(
    FixedOffset='pytz.FixedOffset',
    timezone='pytz.timezone',
    utc='pytz.utc',
    UnknownTimeZoneError='pytz.UnknownTimeZoneError')

json = API(
    dumps='json.dumps',
    dump='json.dump',
    loads='wolframclient.utils.json.loads',
    load='json.load',
    JSONDecodeError='json.decoder.JSONDecodeError')

zlib = API(compressobj='zlib.compressobj', decompressobj='zlib.decompressobj')

os = API(
    X_OK='os.X_OK',
    access='os.access',
    expandvars='os.path.expandvars',
    expanduser='os.path.expanduser',
    dirname='os.path.dirname',
    path_join='os.path.join',
    isfile='os.path.isfile',
    isdir='os.path.isdir',
    exists='os.path.exists',
    makedirs='os.makedirs',
    environ='os.environ',
    listdir='os.listdir')

requests = API(
    get='requests.get',
    post='requests.post',
    request='requests.request',
    Request='requests.Request',
    Response='requests.Response',
    Session='requests.Session')

oauth = API(
    Client='oauthlib.oauth1.Client',
    SIGNATURE_HMAC='oauthlib.oauth1.SIGNATURE_HMAC',
    SIGNATURE_TYPE_AUTH_HEADER='oauthlib.oauth1.SIGNATURE_TYPE_AUTH_HEADER',
)

pip = API(
    main=('pip.main', 'pip._internal.main'),
    get_installed_distributions=(
        'pip.get_installed_distributions',
        'pip.utils.get_installed_distributions',
        'pip._internal.utils.misc.get_installed_distributions',
    ),
    running_under_virtualenv=(
        'pip.locations.running_under_virtualenv',
        'pip._internal.locations.running_under_virtualenv',
    ))

zmq = API(
    Context='zmq.Context',
    PUSH='zmq.PUSH',
    PULL='zmq.PULL',
    PAIR='zmq.PAIR',
    SUB='zmq.SUB',
    SUBSCRIBE='zmq.SUBSCRIBE',
    NOBLOCK='zmq.NOBLOCK',
    LAST_ENDPOINT='zmq.LAST_ENDPOINT',
    Again='zmq.Again')

time = API(perf_counter=('time.perf_counter', 'time.time'), sleep='time.sleep')

futures = API(ThreadPoolExecutor='concurrent.futures.ThreadPoolExecutor')

asyncio = API(
    create_task='wolframclient.utils.asyncio.create_task',
    ensure_future='asyncio.ensure_future',
    wrap_future='asyncio.wrap_future',
    get_event_loop='asyncio.get_event_loop',
    new_event_loop='asyncio.new_event_loop',
    Queue='asyncio.Queue',
    shield='asyncio.shield',
    CancelledError='asyncio.CancelledError',
    wait='asyncio.wait',
    FIRST_COMPLETED='asyncio.FIRST_COMPLETED',
    gather='asyncio.gather',
    Future='asyncio.Future',
    sleep='asyncio.sleep',
    BytesIO='asyncio.io.BytesIO')

urllib = API(
    urlparse=('urllib.parse.urlparse', 'urlparse.urlparse'),
    parse_qs=(
        'urllib.parse.parse_qs',
        'urlparse.parse_qs',
    ),
    urlencode=('urllib.parse.urlencode', 'urllib.urlencode'),
    quote_plus=('urllib.parse.quote_plus', 'urllib.quote_plus'),
    quote=('urllib.parse.quote', 'urllib.quote'))

atexit = API(register='atexit.register', unregister='atexit.unregister')

numpy = API(
    array='numpy.array',
    ndarray='numpy.ndarray',
    arange='numpy.arange',
    int8='numpy.int8',
    int16='numpy.int16',
    int32='numpy.int32',
    int64='numpy.int64',
    uint8='numpy.uint8',
    uint16='numpy.uint16',
    uint32='numpy.uint32',
    uint64='numpy.uint64',
    float16='numpy.float16',
    float32='numpy.float32',
    float64='numpy.float64',
    float128='numpy.float128',
    complex64='numpy.complex64',
    complex128='numpy.complex128',
    nan='numpy.nan',
    dtype='numpy.dtype',
    frombuffer='numpy.frombuffer',
    reshape='numpy.reshape',
    integer='numpy.integer',
    floating='numpy.floating',
    complexfloating='numpy.complexfloating',
    frexp='numpy.frexp',
)

multiprocessing = API(Lock='multiprocessing.Lock')

PIL = API(
    Image='PIL.Image.Image',
    fromarray='PIL.Image.fromarray',
    open='PIL.Image.open',
)

base64 = API(b64encode='base64.b64encode')

pandas = API(
    Series='pandas.Series',
    DataFrame='pandas.DataFrame',
    DatetimeIndex='pandas.DatetimeIndex',
    TimedeltaIndex='pandas.TimedeltaIndex',
    MultiIndex='pandas.MultiIndex',
    SparseSeries='pandas.SparseSeries',
    SparseArray='pandas.SparseArray',
    bdate_range='pandas.bdate_range',
)

aiohttp = API(
    Response='aiohttp.web.Response',
    ClientResponse='aiohttp.ClientResponse',
    CIMultiDict='aiohttp.multipart.CIMultiDict',
    RouteTableDef='aiohttp.web.RouteTableDef',
    Application='aiohttp.web.Application',
    run_app='aiohttp.web.run_app',
    BytesPayload='aiohttp.BytesPayload',
    ClientSession='aiohttp.ClientSession',
    FormData='aiohttp.FormData',
    StringPayload='aiohttp.StringPayload',
)

ssl = API(SSLContext='ssl.SSLContext', )

externalevaluate = API(
    execute_from_file='wolframclient.utils.externalevaluate.execute_from_file',
    start_zmq_loop='wolframclient.utils.externalevaluate.start_zmq_loop',
)
