# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from aiohttp import web

from wolframclient.cli.utils import SimpleCommand
from wolframclient.evaluation import WolframLanguageAsyncSession, WolframEvaluatorPool
from wolframclient.language import wl


async def generate_http_response(session, request, expression):
    return await session.evaluate(
        wl.GenerateHTTPResponse(expression, request)(
            ("BodyByteArray", "Headers", "StatusCode")))

def WolframEngine(path, poolsize = 1, **opts):
    if poolsize <= 1:
        return WolframLanguageAsyncSession(path, **opts)
    return WolframEvaluatorPool(path, poolsize = poolsize, **opts)

class Command(SimpleCommand):
    """ Run test suites from the tests modules.
    A list of patterns can be provided to specify the tests to run.
    """

    port = 18000
    poolsize = 4

    def get_web_app(self, session):

        routes = web.RouteTableDef()

        @routes.get('/')
        async def hello(request):
            response = await generate_http_response(session, request, wl.APIFunction({"x": "String"}))
            return web.Response(
                body=response['BodyByteArray'],
                status=response['StatusCode'],
                headers=dict(rule.args for rule in response['Headers']))

        app = web.Application()
        app.add_routes(routes)

        return app

    def exception_handler(self, exception, context):
        pass

    def handle(self):

        session = WolframEngine(
            '/Applications/Mathematica.app/Contents/MacOS/WolframKernel', poolsize = self.poolsize)
        web.run_app(self.get_web_app(session), port=self.port)
