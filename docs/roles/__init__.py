
# -*- coding: utf-8 -*-
from roles.wl import wl_reference_role

def setup(app):
    app.add_role('wl', wl_reference_role)
    return
