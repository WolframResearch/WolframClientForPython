# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import sys
import traceback
from functools import wraps

from wolframclient.language import wl
from wolframclient.language.exceptions import WolframLanguageException
from wolframclient.serializers import DEFAULT_FORMAT, export
from wolframclient.utils.encoding import force_text, safe_force_text

DEFAULT_UNKNOWN_FAILURE = {
    'wxf':
    b'8:f\x02s\x07FailureS\x0dPythonFailureA\x01-S\x0fMessageTemplateS\x1aUnexpected error occurred.',
    'wl':
    b'Failure["PythonFailure", <|"MessageTemplate" -> "Unexpected error occurred."|>]',
}


def safe_wl_execute(function,
                    args=(),
                    opts={},
                    export_opts={},
                    exception_class=WolframLanguageException):

    __traceback_hidden_variables__ = True

    try:
        return export(function(*args, **opts), **export_opts)
    except Exception as export_exception:
        try:
            try:

                # The user can provide an exception class, and it can be broken, in which case we are running another
                # try / except to return errors that are happening during class serialization

                if isinstance(export_exception, WolframLanguageException):
                    try:
                        export_exception.set_traceback(*sys.exc_info())
                        return export(export_exception, **export_opts)
                    except Exception:
                        pass

                if exception_class is WolframLanguageException:
                    return export(
                        WolframLanguageException(
                            export_exception, exec_info=sys.exc_info()),
                        **export_opts)

                # A custom error class might fail, if this is happening then we can try to use the built in one
                return export(
                    exception_class(
                        export_exception, exec_info=sys.exc_info()),
                    **export_opts)
            except Exception as exception_export_err:
                return export(
                    WolframLanguageException(
                        exception_export_err, exec_info=sys.exc_info()),
                    target_format=export_opts.get('target_format',
                                                  DEFAULT_FORMAT),
                    encoder=
                    'wolframclient.serializers.encoders.builtin.encoder',
                )

        except Exception as unknown_exception:

            # This is the last resort.
            # Everything went wrong, including the code that was supposed to return a traceback, or the custom
            # normalizer is doing something it should not. This should never happen.
            try:
                return export(
                    wl.Failure(
                        "PythonFailure", {
                            "MessageTemplate":
                            safe_force_text(unknown_exception),
                            "MessageParameters": {},
                            "FailureCode":
                            safe_force_text(
                                unknown_exception.__class__.__name__),
                            "Traceback":
                            force_text(traceback.format_exc())
                        }),
                    target_format=export_opts.get('target_format',
                                                  DEFAULT_FORMAT),
                    encoder=
                    'wolframclient.serializers.encoders.builtin.encoder',
                )
            except Exception:
                # Something went worst.
                # this might happen with import errors / syntax errors in third party pluging that are loading the
                # exporter and doing some real damage to the dispatcher we are using.
                return DEFAULT_UNKNOWN_FAILURE[export_opts.get(
                    'target_format', DEFAULT_FORMAT)]


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
