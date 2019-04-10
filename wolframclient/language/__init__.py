# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language.expression import WLSymbolFactory, WLInputExpression

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

Represent symbols in various contexts::

    >>> wl.Developer.PackedArrayQ
    Developer`PackedArrayQ

    >>> wl.Global.f
    Global`f

Specify a context and a subcontext::

    >>> wl.MyContext.MySubContext.SymbolName
    MyContext`MySubContext`SymbolName


"""

System = wl.System
"""A factory of :class:`~wolframclient.language.expression.WLSymbol` instances having ``System``` context.

See :class:`~wolframclient.language.expression.WLSymbolFactory` for more details.

Represent a symbol in the System context::

    >>> System.ImageIdentify
    System`ImageIdentify

"""

Global = wl.Global
"""A factory of :class:`~wolframclient.language.expression.WLSymbol` instances having ``Global``` context.

See :class:`~wolframclient.language.expression.WLSymbolFactory` and
:class:`~wolframclient.language.expression.WLSymbolFactory` for more details.

Represent a symbol in the Global context::

    >>> Global.mySymbol
    Global`mySymbol

Represent a function call to a function::

    >>> Global.myFunction('foo')
    Global`myFunction['foo']

"""

# Sphinx seems to bug on this one, and picks an outdated the docstring when declared in __init__.
wlexpr = WLInputExpression
""" Represent Wolfram Language expressions with input form strings.

Convenient alias for :class:`~wolframclient.language.expression.WLInputExpression`.

Represent an expression::

    >>> wlexpr('Select[Range[10], EvenQ]')
    (Select[Range[10], EvenQ])

Represent a pure function that squares an input argument::

    >>> wlexpr('# ^ 2 &' )
    (# ^ 2 &)

"""


__all__ = ['wl', 'System', 'Global', 'wlexpr']
