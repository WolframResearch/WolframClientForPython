# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.serializers.wxfencoder.wxfencoder import (
    DefaultWXFEncoder, NotEncodedException, WXFEncoder)


class WXFExprProvider(object):
    '''
    Expression provider pull instances of WXFExpr from instances of `WXFEncoder`.

    `WXFExprProvider` can be initialized with an encoder. If none is provided the
    default class `DefaultWXFEncoder` is used to instantiate one. It is possible
    to add extra encoder using `add_encoder`. The order in which encoders are called
    is the one in which they were added. Note that for performance reasons, it is
    recommended to have the default encoder be first, as such one should not initialize
    a provider with an encoder except if the default one is not suited for ones needs.

    An optional `default` function can be passed to the provider at initialization. It
    is used in last resort if no encoder handled the object. It is applied to the
    object and is expected to transform it to something serializable
    e.g: a string using `default=repr`.

    One must carefully design the `default` function to avoid stack overflow.
    '''

    def __init__(self, encoder=None, default=None):
        self.encoders = []
        if encoder is not None:
            self.add_encoder(encoder)
        else:
            self.add_encoder(DefaultWXFEncoder())

        self.default = default

    def add_encoder(self, *encoders):
        ''' Add a new encoder to be called last if all others failed to encode an object. '''
        for encoder in encoders:
            if isinstance(encoder, WXFEncoder):
                self.encoders.append(encoder)
                encoder._provider = self
            else:
                raise TypeError('Invalid encoder.')
        return self

    def provide_wxfexpr(self, o):
        ''' Main function, a generator of wxf expr.'''
        for wxfexpr in self._iter(o):
            yield wxfexpr

    def _iter(self, o, use_default=True):
        ''' Try to encode a given expr using encoders, if none was able to
        process the expr and a `default` function was provided, applies it
        and try again. Otherwise fail.
        '''
        for encoder in self.encoders:
            try:
                for sub in encoder._encode(o):
                    yield sub
                return
            # the expression was not encoded. Moving on to the next encoder.
            except NotEncodedException:
                pass

        if use_default and self.default is not None:
            for sub in self._iter(self.default(o), use_default=False):
                yield sub
        else:
            raise TypeError('Object of type %s is not serializable to WXF.' %
                            o.__class__.__name__)
