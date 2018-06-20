# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import ExpressionFactory

wl = ExpressionFactory()
"""A factory of :class:`WLSymbol<wolframclient.language.expression.WLSymbol>` instances without any particular context.

This instance of :class:`ExpressionFactory<wolframclient.language.expression.ExpressionFactory>` is conveniently used 
by calling its attributes. The following code represents various Wolfram Language expressions::

    # Now
    wl.Now
    # Quantity[3, "Hours"]
    wl.Quanity(3, "Hours")
    # Select[PrimeQ, {1,2,3,4}]
    wl.Select(wl.PrimeQ, [1, 2, 3, 4])
"""

system = ExpressionFactory('System')
"""A factory of :class:`WLSymbol<wolframclient.language.expression.WLSymbol>` instances having ``System``` context.

See :func:`wl<wolframclient.language.expression.ExpressionFactory>` and 
:class:`ExpressionFactory<wolframclient.language.expression.ExpressionFactory>` for more details."""


__all__ = ['wl', 'system']
