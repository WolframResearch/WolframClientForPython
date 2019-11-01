from __future__ import absolute_import, print_function, unicode_literals

import asyncio
import functools


def run_in_loop(cor, loop=None):
    @functools.wraps(cor)
    def wrapped(*args, **kwargs):
        return get_event_loop(loop).run_until_complete(cor(*args, **kwargs))

    return wrapped


def get_event_loop(loop=None):
    try:
        return loop or asyncio.get_event_loop()
    except RuntimeError:
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        return loop


if hasattr(asyncio, "run"):
    run = asyncio.run
else:

    def run(main):
        loop = get_event_loop()
        return loop.run_until_complete(main)


if hasattr(asyncio, "create_task"):
    create_task = asyncio.create_task
else:

    def create_task(coro):
        """ ensure_future using get_event_loop, so that it behaves similarly to
        create_task, and gets the same signature.
        """
        return asyncio.ensure_future(coro, loop=asyncio.get_event_loop())
