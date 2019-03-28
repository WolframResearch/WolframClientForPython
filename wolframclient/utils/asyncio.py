# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import asyncio
import functools

from wolframclient.utils.functional import first, iterate


def run_in_loop(cor, loop=None):
    @functools.wraps(cor)
    def wrapped(*args, **kwargs):
        return get_event_loop(loop).run_until_complete(cor(*args, **kwargs))

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


if hasattr(asyncio, 'create_task'):
    create_task = asyncio.create_task
else:

    def create_task(coro):
        """ ensure_future using get_event_loop, so that it behaves similarly to
        create_task, and gets the same signature.
        """
        return asyncio.ensure_future(coro, loop=asyncio.get_event_loop())
