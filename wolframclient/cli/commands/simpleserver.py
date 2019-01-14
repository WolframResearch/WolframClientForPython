# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import os
import shutil
import tempfile
import uuid

from aiohttp import web
from aiohttp.multipart import CIMultiDict

from wolframclient.cli.utils import SimpleCommand
from wolframclient.evaluation import (WolframEvaluatorPool,
                                      WolframLanguageAsyncSession)
from wolframclient.language import wl, wlexpr
from wolframclient.utils import six
from wolframclient.utils.api import asyncio
from wolframclient.utils.decorators import to_dict
from wolframclient.utils.encoding import force_text
from wolframclient.utils.functional import composition, first


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
def aiohttp_request_to_response(request, post):
    yield 'Method', request.method,
    yield 'Scheme', request.url.scheme,
    yield 'Domain', request.url.host,
    yield 'Port', force_text(request.url.port),
    yield 'PathString', request.url.path,
    yield 'QueryString', request.url.query_string,
    yield 'Headers', tuple(wl.Rule(k, v) for k, v in request.headers.items())
    yield 'MultipartElements', tuple(
        wl.Rule(k, to_multipart(v)) for k, v in post.items())


async def generate_http_response(session, request, expression):
    wl_req = aiohttp_request_to_response(request, await request.post())
    return await session.evaluate(
        wl.GenerateHTTPResponse(expression, wl_req)(("BodyByteArray",
                                                     "Headers", "StatusCode")))


class Command(SimpleCommand):
    """ Run test suites from the tests modules.
    A list of patterns can be provided to specify the tests to run.
    """

    def add_arguments(self, parser):
        parser.add_argument('expressions', nargs='*', type=str)
        parser.add_argument(
            '--get',
            help='Insert the string to Get.',
            action='append',
            default=None)
        parser.add_argument('--port', default=18000, help='Insert the port.')
        parser.add_argument(
            '--kernel',
            default=
            '/Applications/Mathematica.app/Contents/MacOS/WolframKernel',
            help='Insert the kernel path.')
        parser.add_argument(
            '--poolsize',
            default=1,
            help='Insert the kernel pool size.',
            type=int)
        parser.add_argument(
            '--autoreload',
            default=False,
            help='Insert the server should autoreload the WL input expression.',
            action='store_true')
        parser.add_argument(
            '--preload',
            default=False,
            help=
            'Insert the server should should start the kernels immediately.',
            action='store_true')

    def create_session(self, path, poolsize=1, **opts):
        if poolsize <= 1:
            return WolframLanguageAsyncSession(path, **opts)
        return WolframEvaluatorPool(path, poolsize=poolsize, **opts)

    def create_handler(self, expressions, get, autoreload):

        exprs = (*map(wlexpr, expressions), *map(
            autoreload and composition(wl.Get, wl.Once) or wl.Get, get or ()))

        if not exprs:
            return wl.HTTPResponse("<h1>It works!</h1>")
        elif len(exprs) == 1:
            return first(exprs)
        return wl.CompoundExpression(*exprs)

    def get_web_app(self, expressions, kernel, poolsize, preload, **opts):

        session = self.create_session(kernel, poolsize=poolsize)
        handler = self.create_handler(expressions, **opts)

        routes = web.RouteTableDef()

        @routes.route('*', '/{tail:.*}')
        async def hello(request):
            response = await generate_http_response(session, request, handler)
            return web.Response(
                body=response.get('BodyByteArray', b''),
                status=response.get('StatusCode', 200),
                headers=CIMultiDict(
                    rule.args for rule in response.get('Headers', ())))

        app = web.Application()
        app.add_routes(routes)

        if preload:
            asyncio.ensure_future(session.start())

        return app

    def exception_handler(self, exception, context):
        pass

    def handle(self, port, **opts):
        web.run_app(self.get_web_app(**opts), port=port)
