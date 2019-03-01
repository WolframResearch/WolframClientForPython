from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.api import aiohttp, requests
from wolframclient.utils.encoding import force_text

__all__ = ['wrap_response']


class HTTPResponseAdapterBase(object):
    """ Unify various request classes as a unique API. """

    asynchronous = False

    def __init__(self, httpresponse):
        self.response = httpresponse

    def response_object(self):
        return self.response

    def status(self):
        """ HTTP status code """
        return self.response.status_code

    def json(self):
        """ Request body as a json object """
        return self.response.json()

    def text(self):
        """ Request body as decoded text. """
        return self.response.text

    def content(self):
        """ Request body as raw bytes """
        return self.response.content

    def url(self):
        """ String URL. """
        return force_text(self.response.url)

    def headers(self):
        """ Headers as a dict. """
        return self.response.headers


class RequestsHTTPRequestAdapter(HTTPResponseAdapterBase):
    pass


class AIOHttpHTTPRequestAdapter(HTTPResponseAdapterBase):

    asynchronous = True

    def status(self):
        return self.response.status

    async def json(self):
        return await self.response.json()

    async def text(self):
        return await self.response.text()

    async def content(self):
        return await self.response.read()


def wrap_response(response):
    if isinstance(response, requests.Response):
        return RequestsHTTPRequestAdapter(response)
    elif isinstance(response, aiohttp.ClientResponse):
        return AIOHttpHTTPRequestAdapter(response)
    else:
        raise ValueError(
            'No adapter found for HTTP response class %s' % response.__class__)
