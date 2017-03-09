# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.importutils import API

DEFAULT_FORMAT = 'wl'

available_formats = API(
    wl = 'wolframclient.serializers.wl.WLSerializer',
    wxf = 'wolframclient.serializers.wxf.WXFSerializer',
)

def export(data, stream = None, format = 'wl', **options):
    if not format in available_formats:
        raise ValueError('Invalid export format %s. Choices are: %s' % (
            format,
            ', '.join(available_formats.keys())
        ))
    return available_formats[format](**options).export(data, stream = stream)