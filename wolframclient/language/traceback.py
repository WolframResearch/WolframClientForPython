# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import re

from wolframclient.language import wl
from wolframclient.utils import six
from wolframclient.utils.decorators import to_tuple
from wolframclient.utils.encoding import force_text, safe_force_text
from wolframclient.utils.functional import iterate


def serialize_traceback(exc_type, exc_value, tb, **opts):
    return wl.OpenerView([
        wl.Row([
            safe_force_text(exc_type.__name__), " ",
            safe_force_text(exc_value)
        ]),
        wl.Style(
            wl.Column(_serialize_traceback(exc_type, exc_value, tb, **opts)),
            FontFamily="Courier")
    ], True)


def _serialize_traceback(exc_type, exc_value, tb, **opts):

    frames = _get_traceback_frames(tb, exc_value, **opts)

    for i, frame in enumerate(frames):
        for sub in _serialize_frames(
                is_opened=i + 1 > len(frames) - 2, **frame):
            yield sub


def _serialize_variables(variables):

    hidden = variables.get('__traceback_hidden_variables__', ())

    if hidden is True:
        return

    if not isinstance(hidden, (tuple, list)):
        hidden = ()

    variables = tuple((safe_force_text(key), safe_force_text(value))
                      for key, value in variables.items() if not key in hidden)

    if variables:
        yield wl.OpenerView([
            "Local variables",
            wl.Grid(
                iterate(
                    (("Key", "Value"), ),
                    variables,
                ),
                Background=[None, [wl.LightGray]],
                Alignment=wl.Left,
                Frame=wl.LightGray)
        ])
    else:
        yield "No local variables"


def _paginate(i, line):
    return '%s.  %s' % (force_text(i).rjust(4), line)


def _serialize_frames(filename,
                      function,
                      pre_context,
                      post_context,
                      context_line,
                      variables,
                      lineno,
                      pre_context_lineno,
                      is_opened=False,
                      **opts):

    if filename:
        description = wl.Row([
            wl.Button(
                wl.Style(
                    filename,
                    wl.RGBColor(0.25, 0.48, 1),
                    wl.Small,
                    FontFamily="Courier"),
                wl.If(wl.FileExistsQ(filename), wl.SystemOpen(filename)),
                Appearance="Frameless"), ' in ', function
        ])
    else:
        description = function

    yield wl.OpenerView([
        description,
        wl.Column(
            iterate((wl.Column(
                iterate(
                    (_paginate(pre_context_lineno + i, l)
                     for i, l in enumerate(pre_context)),
                    [
                        wl.Item(
                            _paginate(lineno, context_line),
                            Background=wl.LightYellow)
                    ],
                    (_paginate(lineno + i + 1, l)
                     for i, l in enumerate(post_context)),
                ),
                Background=[[wl.GrayLevel(0.95),
                             wl.GrayLevel(1)]],
                Frame=wl.LightGray), ), _serialize_variables(variables)))
    ], is_opened)


@to_tuple
def _get_traceback_frames(traceback, exc_value, context_lines=7):
    def explicit_or_implicit_cause(exc_value):
        explicit = getattr(exc_value, '__cause__', None)
        implicit = getattr(exc_value, '__context__', None)
        return explicit or implicit

    # Get the exception and all its causes
    exceptions = []

    while exc_value:
        exceptions.append(exc_value)
        exc_value = explicit_or_implicit_cause(exc_value)

    # No exceptions were supplied to ExceptionReporter
    if exceptions:

        # In case there's just one exception, take the traceback from self.tb
        exc_value = exceptions.pop()
        tb = traceback if not exceptions else exc_value.__traceback__

        while tb is not None:
            # Support for __traceback_hide__ which is used by a few libraries
            # to hide internal frames.
            if tb.tb_frame.f_locals.get('__traceback_hide__'):
                tb = tb.tb_next
                continue
            filename = tb.tb_frame.f_code.co_filename
            function = tb.tb_frame.f_code.co_name
            lineno = tb.tb_lineno - 1
            loader = tb.tb_frame.f_locals.get(
                '__loader__') or tb.tb_frame.f_globals.get('__loader__')
            module_name = tb.tb_frame.f_globals.get('__name__') or ''

            pre_context_lineno, pre_context, context_line, post_context = _get_lines_from_file(
                filename,
                lineno,
                context_lines,
                loader,
                module_name,
            )

            if pre_context_lineno is None:
                pre_context_lineno = lineno
                pre_context = []
                context_line = '<source code not available>'
                post_context = []

            yield {
                'filename': filename and force_text(filename) or None,
                'function': function and force_text(function) or None,
                'lineno': lineno + 1,
                'variables': tb.tb_frame.f_locals,
                'pre_context': pre_context,
                'context_line': context_line,
                'post_context': post_context,
                'pre_context_lineno': pre_context_lineno + 1,
            }

            # If the traceback for current exception is consumed, try the
            # other exception.
            if not tb.tb_next and exceptions:
                exc_value = exceptions.pop()
                tb = exc_value.__traceback__
            else:
                tb = tb.tb_next


def _get_lines_from_file(filename,
                         lineno,
                         context_lines,
                         loader=None,
                         module_name=None):
    """
    Return context_lines before and after lineno from file.
    Return (pre_context_lineno, pre_context, context_line, post_context).
    """
    source = None

    if loader is not None and hasattr(loader, "get_source"):
        try:
            source = loader.get_source(module_name)
        except ImportError:
            pass
        if source is not None:
            source = source.splitlines()

    if source is None:
        try:
            with open(filename, 'rb') as fp:
                source = fp.read().splitlines()
        except (OSError, IOError):
            pass

    if source is None:
        return None, [], None, []

    # If we just read the source from a file, or if the loader did not
    # apply tokenize.detect_encoding to decode the source into a
    # string, then we should do that ourselves.
    if isinstance(source[0], six.binary_type):
        encoding = 'ascii'
        for line in source[:2]:
            # File coding may be specified. Match pattern from PEP-263
            # (http://www.python.org/dev/peps/pep-0263/)
            match = re.search(br'coding[:=]\s*([-\w.]+)', line)
            if match:
                encoding = match.group(1).decode('ascii')
                break
        source = [force_text(sline, encoding, 'replace') for sline in source]

    lower_bound = max(0, lineno - context_lines)
    upper_bound = lineno + context_lines

    pre_context = source[lower_bound:lineno]
    context_line = source[lineno]
    post_context = source[lineno + 1:upper_bound]

    return lower_bound, pre_context, context_line, post_context
