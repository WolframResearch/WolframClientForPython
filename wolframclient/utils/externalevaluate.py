# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import ast
import logging
import os
import sys

from wolframclient.deserializers import binary_deserialize
from wolframclient.language import wl
from wolframclient.language.decorators import to_wl
from wolframclient.language.side_effects import side_effect_logger
from wolframclient.serializers import export
from wolframclient.utils import six
from wolframclient.utils.api import zmq
from wolframclient.utils.datastructures import Settings
from wolframclient.utils.encoding import force_text
from wolframclient.utils.functional import last
from wolframclient.utils.importutils import import_string

HIDDEN_VARIABLES = [
    '__loader__', '__builtins__', '__traceback_hidden_variables__',
    'absolute_import', 'print_function', 'unicode_literals'
]

EXPORT_KWARGS = {
    'target_format': 'wxf',
    'allow_external_objects': True,
}


class UnprintableContext(dict):
    def __repr__(self):
        return '<evaluation context>'


def execute_from_file(path, *args, **opts):
    with open(path, 'r') as f:
        return execute_from_string(force_text(f.read()), *args, **opts)


def execute_from_string(string, context=UnprintableContext()):

    __traceback_hidden_variables__ = [
        'context', 'current', '__traceback_hidden_variables__'
    ]

    #this is creating a custom __loader__ that is returning the source code
    #traceback serializers is inspecting global variables and looking for a standard loader that can return source code.

    current = UnprintableContext(context or ())
    current['__loader__'] = Settings(
        get_source=lambda module, code=string: code)
    current['__traceback_hidden_variables__'] = HIDDEN_VARIABLES

    expressions = list(
        compile(
            string,
            filename='<unknown>',
            mode='exec',
            flags=ast.PyCF_ONLY_AST | unicode_literals.compiler_flag).body)

    if not expressions:
        return

    result = None

    if isinstance(last(expressions), ast.Expr):
        result = expressions.pop(-1)

    if expressions:
        exec(compile(ast.Module(expressions), '', 'exec'), current)

    if result:
        result = eval(
            compile(ast.Expression(result.value), '', 'eval'), current)
    else:
        result = wl.Null

    if context is not None:
        context.update(current)

    return result


class SideEffectSender(logging.Handler):
    def emit(self, record):
        if isinstance(sys.stdout, StdoutProxy):
            sys.stdout.send_side_effect(record.msg)


side_effect_logger.addHandler(SideEffectSender())


class SocketWriter:
    def __init__(self, socket):
        self.socket = socket

    def write(self, bytes):
        self.socket.send(bytes)


class StdoutProxy:

    keep_listening = wl.ExternalEvaluate.Private.PythonKeepListening

    def __init__(self, stream):
        self.stream = stream
        self.clear()

    def clear(self):
        self.current_line = []
        self.lines = []

    def write(self, message):
        messages = force_text(message).split("\n")

        if len(messages) == 1:
            self.current_line.extend(messages)
        else:
            self.current_line.append(messages.pop(0))
            rest = messages.pop(-1)

            self.lines.extend(messages)
            self.flush()
            if rest:
                self.current_line.append(rest)

    def flush(self):
        if self.current_line or self.lines:
            self.send_lines(''.join(self.current_line), *self.lines)
            self.clear()

    def send_lines(self, *lines):
        if len(lines) == 1:
            return self.send_side_effect(wl.Print(*lines))
        elif lines:
            return self.send_side_effect(
                wl.CompoundExpression(*map(wl.Print, lines)))

    def send_side_effect(self, expr):
        self.stream.write(export(self.keep_listening(expr), **EXPORT_KWARGS))


def evaluate_message(context,
                     input=None,
                     return_type=None,
                     function=None,
                     is_module=False,
                     args=None,
                     **opts):

    __traceback_hidden_variables__ = True

    if function and args is not None:
        #then we have a function call to do
        #first get the function object we need to call
        if is_module:
            func = import_string(function)
        else:
            func = execute_from_string(function, context)
        #get the full argument types (possibly calling a serialization function if necessary)
        #finally call the function and assign the output
        return func(*args)

    if isinstance(input, six.string_types):
        result = execute_from_string(input, context)

        if return_type == 'string':
            # bug 354267 repr returns a 'str' even on py2 (i.e. bytes).
            return force_text(repr(result))

        return result


@to_wl(**EXPORT_KWARGS)
def handle_message(socket, context=UnprintableContext()):

    __traceback_hidden_variables__ = True

    message = binary_deserialize(socket.recv())
    result = evaluate_message(context=context, **message)

    sys.stdout.flush()
    return result


def start_zmq_instance(port=None, write_to_stdout=True, **opts):

    # make a reply socket
    sock = zmq.Context.instance().socket(zmq.PAIR)
    #now bind to a open port on localhost
    if port:
        sock.bind('tcp://127.0.0.1:%s' % port)
    else:
        sock.bind_to_random_port('tcp://127.0.0.1')

    if write_to_stdout:
        sys.stdout.write(force_text(sock.getsockopt(zmq.LAST_ENDPOINT)))
        sys.stdout.write(os.linesep)  #writes \n
        sys.stdout.flush()

    return sock


def start_zmq_loop(message_limit=float('inf'), redirect_stdout=True, **opts):
    socket = start_zmq_instance(**opts)

    stream = SocketWriter(socket)

    messages = 0

    if redirect_stdout:
        sys.stdout = StdoutProxy(stream)

    #now sit in a while loop, evaluating input
    while messages < message_limit:
        stream.write(handle_message(socket))
        messages += 1

    if redirect_stdout:
        sys.stdout = sys.__stdout__
