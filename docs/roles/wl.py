# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
from docutils import nodes, utils
from docutils.parsers.rst import roles

wl_ref_url = 'http://reference.wolfram.com/language/ref/%s.html'
wxf_ref_url = 'http://reference.wolfram.com/language/tutorial/WXFFormatDescription.html'

def wl_reference_role(role_name, rawtext, wl_symbol, lineno, inliner,
                       options={}, content=[]):
    if wl_symbol == 'WXF':
        symbol_ref_url = wxf_ref_url
    else:
        symbol_ref_url = wl_ref_url % utils.unescape(wl_symbol)
    node = nodes.reference(rawtext, wl_symbol, refuri=symbol_ref_url,
                           classes=['wl', 'external'],
                           **options)
    return [node], []
