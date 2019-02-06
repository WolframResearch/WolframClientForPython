# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import time
from operator import itemgetter

import aiohttp

from wolframclient.cli.utils import SimpleCommand
from wolframclient.evaluation import (WolframEvaluatorPool,
                                      WolframLanguageAsyncSession)
from wolframclient.serializers import export
from wolframclient.utils.asyncio import run_in_loop, wait_all
from wolframclient.utils.encoding import force_text
from wolframclient.utils.functional import iterate


class Command(SimpleCommand):
    """ Run test suites from the tests modules.
    A list of patterns can be provided to specify the tests to run.
    """

    col_size = 20

    def add_arguments(self, parser):
        parser.add_argument(
            '--requests',
            default=100,
            help='Insert the number or request to be done.',
            type=int)
        parser.add_argument(
            '--clients',
            default=4,
            help='Insert the number of clients.',
            type=int)
        parser.add_argument(
            '--url',
            default='http://localhost:18000',
            help='Insert the url to stress.')
        parser.add_argument('--format', default=None)

    async def consumer(self, queue, i):
        results = []
        async with aiohttp.ClientSession() as session:
            while queue:
                t1 = time.time()
                async with session.get(queue.pop()) as resp:
                    bytes_count = len(await resp.content.read())
                    results.append({
                        'time': time.time() - t1,
                        'bytes': bytes_count,
                        'success': resp.status == 200
                    })

        return results

    def generate_tasks(self, requests, clients, url):

        # Create the queue with a fixed size so the producer
        # will block until the consumers pull some items out.
        queue = [url for i in range(requests)]

        for i in range(clients):
            yield self.consumer(queue, i)

    @run_in_loop
    async def wait_for_tasks(self, requests, clients, url):
        # Wait for all of the coroutines to finish.
        results = await wait_all(self.generate_tasks(requests, clients, url))
        results = tuple(iterate(*results))
        return results

    def create_data(self, requests, clients, url):

        yield 'Url', url
        yield 'Clients', clients
        yield 'Requests', requests

        t1 = time.time()

        # Wait for all of the coroutines to finish.
        results = self.wait_for_tasks(requests, clients, url)

        s = sum(map(itemgetter('time'), results))
        kb = sum(map(itemgetter('bytes'), results)) / 1024
        l = len(results)

        t2 = time.time() - t1

        assert l == requests

        yield 'Requests OK', sum(map(itemgetter('success'), results))
        yield 'Requests/sec', 1 / (t2 / l)

        yield 'Total time', t2
        yield 'Avg time', t2 / l

        yield 'Total Kb', kb
        yield 'Avb req Kb', kb / l
        yield 'Kb/sec', kb / t2

        yield 'Client total time', s
        yield 'Client avg time', s / l

    def table_line(self, *iterable):
        self.print(*(force_text(c).ljust(self.col_size) for c in iterable))

    def handle(self, format, **opts):
        data = self.create_data(**opts)
        if format:
            self.print(export(dict(data), target_format=format))
        else:
            for k, v in data:
                if isinstance(v, float):
                    v = '%.4f' % v
                self.table_line(k, v)
