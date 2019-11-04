from __future__ import absolute_import, print_function, unicode_literals

import asyncio

from wolframclient.utils.decorators import decorate


def get_event_loop(loop=None):
    try:
        return loop or asyncio.get_event_loop()
    except RuntimeError:
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        return loop


def run(coro):
    return get_event_loop().run_until_complete(coro)


run_in_loop = decorate(run)


def create_task(coro):
    """ ensure_future using get_event_loop, so that it behaves similarly to
    create_task, and gets the same signature.
    """
    return asyncio.ensure_future(coro, loop=get_event_loop())
