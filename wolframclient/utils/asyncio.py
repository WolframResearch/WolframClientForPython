# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import functools
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


def run_in_loop(cor, loop=None):
    loop = get_event_loop(loop)

    @functools.wraps(cor)
    def wrapped(*args, **kwargs):
        loop.run_until_complete(cor(*args, **kwargs))

    return wrapped


def run_all(args, **opts):
    done = tuple(iterate(*args))
    if done and len(done) > 1:
        return asyncio.ensure_future(asyncio.wait(done), **opts)
    elif done:
        return asyncio.ensure_future(first(done), **opts)
    return done


def get_event_loop(loop=None):
    try:
        return loop or asyncio.get_event_loop()
    except RuntimeError:
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        return loop


@to_tuple
def syncronous_wait_all(*args, loop=None):
    yield from (loop or get_event_loop()).run_until_complete(wait_all(*args))


def to_sync(timeout=None):
    def wrap(cor):
        @functools.wraps(cor)
        def wrapper(*args, **kwargs):
            task = asyncio.create_task(cor(*args, **kwargs))

        return wrapper

    return wrap


def silence(*exceptions):
    def wrap(fn):
        @functools.wraps(fn)
        def wrapper(*args, **kwargs):
            try:
                return fn(*args, **kwargs)
            except tuple(exceptions):
                pass

        return wrapper

    return wrap
