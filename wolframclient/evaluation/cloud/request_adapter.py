from __future__ import absolute_import, print_function, unicode_literals

from aiohttp import ClientResponse
from requests import Response

__all__ = ['wrap_response']


class HTTPResponseAdapterBase(object):
    """ Unify various request classes as a unique API. """

    def __init__(self, httpresponse):
        self.response = httpresponse

    def response_object(self):
        return self.response

    def status(self):
        """ HTTP status code """
        raise NotImplementedError

    def json(self):
        """ Request body as a json object """
        raise NotImplementedError

    def text(self):
        """ Request body as decoded text. """
        raise NotImplementedError

    def content(self):
        """ Request body as raw bytes """
        raise NotImplementedError

    def url(self):
        """ String URL. """
        raise NotImplementedError

    def headers(self):
        """ Headers as a dict. """
        raise NotImplementedError

    @property
    def asynchronous(self):
        raise NotImplementedError


class RequestsHTTPRequestAdapter(HTTPResponseAdapterBase):
    def status(self):
        return self.response.status_code

    def json(self):
        return self.response.json()

    def text(self):
        return self.response.text

    def content(self):
        return self.response.content

    def url(self):
        return self.response.url

    def headers(self):
        return self.response.headers

    @property
    def asynchronous(self):
        return False


class AIOHttpHTTPRequestAdapter(HTTPResponseAdapterBase):
    def status(self):
        return self.response.status

    async def json(self):
        return await self.response.json()

    async def text(self):
        return await self.response.text()

    async def content(self):
        return await self.response.read()

    def url(self):
        return str(self.response.url)

    def headers(self):
        return self.response.headers

    @property
    def asynchronous(self):
        return True


def wrap_response(response):
    if isinstance(response, Response):
        return RequestsHTTPRequestAdapter(response)
    elif isinstance(response, ClientResponse):
        return AIOHttpHTTPRequestAdapter(response)
    else:
        raise ValueError(
            'No adapter found for HTTP response class %s' % response.__class__)
