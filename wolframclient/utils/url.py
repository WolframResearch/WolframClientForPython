# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six


def url_join(*fragments):
    """ Join fragments of a URL, dealing with slashes."""
    if len(fragments) == 0:
        return ''
    buff = []
    for fragment in fragments:
        stripped = fragment.strip('/')
        if len(stripped) > 0:
            buff.append(stripped)
            buff.append('/')

    last = fragments[-1]
    # add a trailing '/' if present.
    if len(last) > 0 and last[-1] != '/':
        buff.pop()
    return ''.join(buff)


def evaluation_api_url(server):
    return url_join(server.cloudbase, 'evaluations?_responseform=wxf')


def user_api_url(server, api):
    """Build an API URL from a user name and an API id. """
    if isinstance(api, tuple) or isinstance(api, list):
        if len(api) == 2:
            return url_join(server.cloudbase, 'objects', api[0], api[1])
        else:
            raise ValueError(
                'Target api specified as a tuple must have two elements: the user name, the API name.'
            )
    elif isinstance(api, six.string_types):
        return api
    else:
        raise ValueError('Invalid API description. Expecting string or tuple.')
