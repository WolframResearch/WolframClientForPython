# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from functools import wraps

from wolframclient.language.exceptions import WolframLanguageException
from wolframclient.serializers import export

import sys

def safe_wl_execute(function, args = (), opts = {}, export_opts = {}, exception_class = WolframLanguageException):
    try:
        return export(function(*args, **opts), **export_opts)
    except Exception as e:

        #the user might provide an exception class, that might be broken.
        #in this case we are running another try / except to return errors that are happneing during class serialization

        if isinstance(e, WolframLanguageException):
            try:
                e.set_traceback(*sys.exc_info())
                return export(e, **export_opts)
            except Exception as e:
                pass

        if exception_class is WolframLanguageException:
            return export(WolframLanguageException(e, exec_info = sys.exc_info()), **export_opts)
        try:
            return export(exception_class(e, exec_info = sys.exc_info()), **export_opts)
        except Exception as e:
            return export(WolframLanguageException(e, exec_info = sys.exc_info()), **export_opts)

def to_wl(**export_opts):
    def outer(function):
        @wraps(function)
        def inner(*args, **opts):
            return safe_wl_execute(
                function = function,
                args = args,
                opts = opts,
                export_opts = export_opts,
            )
        return inner
    return outer