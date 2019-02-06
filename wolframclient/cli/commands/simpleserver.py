# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.cli.utils import SimpleCommand
from wolframclient.evaluation import (WolframEvaluatorPool,
                                      WolframLanguageAsyncSession)
from wolframclient.http import aiohttp_wl_view
from wolframclient.language import wl, wlexpr
from wolframclient.utils.api import aiohttp, asyncio
from wolframclient.utils.functional import composition, first, identity


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
            composition(wl.Get, autoreload and identity or wl.Once, wl.Delayed), get or ()))

        if not exprs:
            return wl.HTTPResponse("<h1>It works!</h1>")
        elif len(exprs) == 1:
            return first(exprs)
        return wl.CompoundExpression(*exprs)

    def get_web_app(self, expressions, kernel, poolsize, preload, **opts):

        session = self.create_session(
            kernel, poolsize=poolsize, inputform_string_evaluation=False)
        handler = self.create_handler(expressions, **opts)

        routes = aiohttp.RouteTableDef()

        @routes.route('*', '/{tail:.*}')
        @aiohttp_wl_view(session)
        async def main(request):
            return handler

        app = aiohttp.Application()
        app.add_routes(routes)

        if preload:
            asyncio.ensure_future(session.start())

        return app

    def exception_handler(self, exception, context):
        pass

    def handle(self, port, **opts):
        aiohttp.run_app(self.get_web_app(**opts), port=port)
