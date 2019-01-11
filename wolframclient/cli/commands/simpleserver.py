# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from aiohttp import web

from wolframclient.cli.utils import SimpleCommand
from wolframclient.evaluation import (WolframEvaluatorPool,
                                      WolframLanguageAsyncSession)
from wolframclient.language import wl


async def generate_http_response(session, request, expression):
    return await session.evaluate(
        wl.GenerateHTTPResponse(expression, request)(
            ("BodyByteArray", "Headers", "StatusCode")))


class Command(SimpleCommand):
    """ Run test suites from the tests modules.
    A list of patterns can be provided to specify the tests to run.
    """

    def add_arguments(self, parser):
        parser.add_argument(
            '--get', default=None, help='Insert the string to Get.')
        parser.add_argument('--port', default=18000, help='Insert the port.')
        parser.add_argument(
            '--kernel',
            default=
            '/Applications/Mathematica.app/Contents/MacOS/WolframKernel',
            help='Insert the kernel path.')
        parser.add_argument(
            '--poolsize', default=4, help='Insert the kernel pool size.')
        parser.add_argument(
            '--autoreload',
            default=False,
            help='Insert the server should autoreload the WL input expression.',
            action='store_true')

    def create_session(self, path, poolsize=1, **opts):
        if poolsize <= 1:
            return WolframLanguageAsyncSession(path, **opts)
        return WolframEvaluatorPool(path, poolsize=poolsize, **opts)

    def create_handler(self, get, autoreload):
        if not get:
            return wl.HTTPResponse("<h1>It works!</h1>")
        if autoreload:
            return wl.Get(get)
        return wl.Once(wl.Get(get))

    def get_web_app(self, kernel, poolsize, **opts):

        session = self.create_session(kernel, poolsize=poolsize)
        handler = self.create_handler(**opts)

        routes = web.RouteTableDef()

        @routes.get('/')
        async def hello(request):
            response = await generate_http_response(session, request, handler)
            return web.Response(
                body=response['BodyByteArray'],
                status=response['StatusCode'],
                headers=dict(rule.args for rule in response['Headers']))

        app = web.Application()
        app.add_routes(routes)

        return app

    def exception_handler(self, exception, context):
        pass

    def handle(self, port, **opts):
        web.run_app(self.get_web_app(**opts), port=port)
