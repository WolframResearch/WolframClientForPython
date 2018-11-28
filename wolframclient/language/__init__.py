# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import WLSymbolFactory, WLInputExpression

__all__ = ['wl', 'System', 'Global', 'wlexpr']

wl = WLSymbolFactory()
"""A factory of :class:`~wolframclient.language.expression.WLSymbol` instances without any particular context.

This instance of :class:`~wolframclient.language.expression.WLSymbolFactory` is conveniently used
by calling its attributes. The following code represents various Wolfram Language expressions::

    # Now
    wl.Now
    # Quantity[3, "Hours"]
    wl.Quantity(3, "Hours")
    # Select[PrimeQ, {1,2,3,4}]
    wl.Select(wl.PrimeQ, [1, 2, 3, 4])

It is possible to specify context using the syntax::

    wl.MyContext.MySubContext.SymbolName
"""

System = wl.System
"""A factory of :class:`~wolframclient.language.expression.WLSymbol` instances having ``System``` context.

See :class:`~wolframclient.language.expression.WLSymbolFactory` and
:class:`~wolframclient.language.expression.WLSymbolFactory` for more details."""

Global = wl.Global
"""A factory of :class:`~wolframclient.language.expression.WLSymbol` instances having ``Global``` context.

See :class:`~wolframclient.language.expression.WLSymbolFactory` and
:class:`~wolframclient.language.expression.WLSymbolFactory` for more details."""

wlexpr = WLInputExpression
""" A string wrapper for input string expressions.

Convenient alias for :class:`~wolframclient.language.expression.WLInputExpression`.
"""
