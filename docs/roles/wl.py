# -*- coding: utf-8 -*-
from docutils import nodes, utils
from docutils.parsers.rst import roles

wl_ref_url = 'http://reference.wolfram.com/language/ref/%s.html'

def wl_reference_role(role_name, rawtext, wl_symbol, lineno, inliner,
                       options={}, content=[]):
    symbol_ref_url = wl_ref_url % utils.unescape(wl_symbol)
    node = nodes.reference(rawtext, wl_symbol, refuri=symbol_ref_url,
                           classes=['wl'],
                           **options)
    return [node], []
