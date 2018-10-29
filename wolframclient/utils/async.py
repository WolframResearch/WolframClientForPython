# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from operator import methodcaller

from wolframclient.utils.api import asyncio
from wolframclient.utils.decorators import to_tuple
from wolframclient.utils.functional import first, iterate

async def wait_all(args, **opts):
    done = tuple(iterate(args))
    if done:
        done, _ = await asyncio.wait(done, **opts)
        return tuple(map(methodcaller('result'), done))

    return done

def run_all(args, **opts):
    done = tuple(iterate(*args))
    if done and len(done) > 1:
        return asyncio.ensure_future(asyncio.wait(done), **opts)
    elif done:
        return asyncio.ensure_future(first(done), **opts)
    return done

def get_event_loop(loop = None):
    try:
        return loop or asyncio.get_event_loop()
    except RuntimeError:
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        return loop

@to_tuple
def syncronous_wait_all(*args, loop = None):
    yield from (loop or get_event_loop()).run_until_complete(wait_all(*args))