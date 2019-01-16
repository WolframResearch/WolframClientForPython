# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language import wl
from wolframclient.utils import six
from wolframclient.utils.api import aiohttp
from wolframclient.utils.decorators import to_dict
from wolframclient.utils.encoding import force_text

import os
import uuid
import tempfile
import shutil


def to_multipart(v):
    if isinstance(v, six.string_types):
        return {"ContentString": v, "InMemory": True}

    destdir = os.path.join(tempfile.gettempdir(), force_text(uuid.uuid4()))
    os.mkdir(destdir)

    with open(os.path.join(destdir, v.filename), 'wb') as dest:
        shutil.copyfileobj(v.file, dest)
        return {
            "FileName": dest.name,
            "InMemory": False,
            "OriginalFileName": v.filename
        }


@to_dict
def aiohttp_request_meta(request, post):
    yield 'Method', request.method
    yield 'Scheme', request.url.scheme
    yield 'Domain', request.url.host
    yield 'Port', force_text(request.url.port)
    yield 'PathString', request.url.path
    yield 'QueryString', request.url.query_string
    yield 'Headers', tuple(wl.Rule(k, v) for k, v in request.headers.items())
    yield 'MultipartElements', tuple(wl.Rule(k, to_multipart(v)) for k, v in post.items())


async def generate_http_response(session, request, expression):
    wl_req = aiohttp_request_meta(request, await request.post())

    response = await session.evaluate(
        wl.GenerateHTTPResponse(expression, wl_req)(("BodyByteArray",
                                                     "Headers", "StatusCode")))

    return aiohttp.Response(
        body=response.get('BodyByteArray', b''),
        status=response.get('StatusCode', 200),
        headers=aiohttp.CIMultiDict(
            rule.args for rule in response.get('Headers', ())))
