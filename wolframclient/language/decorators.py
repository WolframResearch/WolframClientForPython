# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import sys
import traceback
from functools import wraps

from wolframclient.language import wl
from wolframclient.language.exceptions import WolframLanguageException
from wolframclient.serializers import DEFAULT_FORMAT, export
from wolframclient.utils.encoding import force_text, safe_force_text


def safe_wl_execute(function,
                    args=(),
                    opts={},
                    export_opts={},
                    exception_class=WolframLanguageException):

    __traceback_hidden_variables__ = True

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

        try:
            if exception_class is WolframLanguageException:
                return export(
                    WolframLanguageException(e, exec_info=sys.exc_info()),
                    **export_opts)

            #a custom error class might fail, if this is happening then we can try to use the built in one
            try:
                return export(
                    exception_class(e, exec_info=sys.exc_info()),
                    **export_opts)
            except Exception as e:
                return export(
                    WolframLanguageException(e, exec_info=sys.exc_info()),
                    **export_opts)

        except Exception as e:

            #this is the last resort.
            #everything went wrong, including the code that was supposed to return a traceback, or the custom normalizer is doing something it should not.
            #this should never happen.

            return export(
                wl.Failure(
                    "PythonFailure", {
                        "MessageTemplate": safe_force_text(e),
                        "MessageParameters": {},
                        "FailureCode": safe_force_text(e.__class__.__name__),
                        "Traceback": force_text(traceback.format_exc())
                    }),
                target_format=export_opts.get('target_format', DEFAULT_FORMAT))


def to_wl(**export_opts):
    def outer(function):
        @wraps(function)
        def inner(*args, **opts):
            return safe_wl_execute(
                function=function,
                args=args,
                opts=opts,
                export_opts=export_opts,
            )

        return inner

    return outer
