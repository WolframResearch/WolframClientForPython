# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.api import asyncio
from wolframclient.utils.asyncio import syncronous_wait_all
from wolframclient.utils.tests import TestCase as BaseTestCase


class TestCase(BaseTestCase):
    def test_wait(self, r=100):
        async def return_ordered(i):
            await asyncio.sleep(0.01)
            return i

        res = syncronous_wait_all(return_ordered(i) for i in range(r))

        self.assertEqual(res, tuple(range(r)))
