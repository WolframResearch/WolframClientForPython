# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals
from roles.wl import wl_reference_role

def setup(app):
    app.add_role('wl', wl_reference_role)
    return
