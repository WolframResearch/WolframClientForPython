# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import time

import aiohttp

from wolframclient.cli.utils import SimpleCommand
from wolframclient.evaluation import (WolframEvaluatorPool,
                                      WolframLanguageAsyncSession)
from wolframclient.utils.asyncio import run_in_loop, wait_all
from wolframclient.utils.functional import flatten, iterate

from operator import itemgetter

class Command(SimpleCommand):
    """ Run test suites from the tests modules.
    A list of patterns can be provided to specify the tests to run.
    """

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

    async def consumer(self, queue, i):
        results = []
        async with aiohttp.ClientSession() as session:
            while queue:
                t1 = time.time()
                async with session.get(queue.pop()) as resp:
                    bytes_count = len(await resp.content.read())
                    assert resp.status == 200
                    results.append({'time': time.time() - t1, 'bytes': bytes_count})

        return results

    def generate_tasks(self, requests, clients, url):

        # Create the queue with a fixed size so the producer
        # will block until the consumers pull some items out.
        queue = [url for i in range(requests)]

        for i in range(clients):
            yield self.consumer(queue, i)

    @run_in_loop
    async def handle(self, requests, clients, url):

        print('Requests', requests)
        print('Clients', clients)
        print('Url', url)

        t1 = time.time()

        # Wait for all of the coroutines to finish.
        results = await wait_all(self.generate_tasks(requests, clients, url))
        results = tuple(iterate(*results))

        s = sum(map(itemgetter('time'), results))
        kb = sum(map(itemgetter('bytes'), results)) / 1024
        l = len(results)

        t2 = time.time() - t1

        assert l == requests

        print('Total Kb', kb)
        print('Avb req Kb', kb / l)
        print('Kb/sec', kb / t2)

        print('Total time', t2)
        print('Avg time', t2 / l)
        print('Reqests/sec', 1 / (t2 / l))

        print('Client total time', s)
        print('Client avg time', s / l)
