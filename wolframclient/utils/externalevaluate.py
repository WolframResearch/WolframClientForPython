# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import ast
import logging
import os
import sys
from itertools import chain

from wolframclient.deserializers import binary_deserialize
from wolframclient.language import wl
from wolframclient.language.decorators import to_wl
from wolframclient.language.side_effects import side_effect_logger
from wolframclient.serializers import export
from wolframclient.utils import six
from wolframclient.utils.api import zmq
from wolframclient.utils.encoding import force_text
from wolframclient.utils.functional import last

if six.PY_35:
    pass
else:
    pass

HIDDEN_VARIABLES = [
    '__loader__', '__builtins__', '__traceback_hidden_variables__',
    'absolute_import', 'print_function', 'unicode_literals'
]

EXPORT_KWARGS = {
    'target_format': 'wxf',
    'allow_external_objects': True,
}


class EvaluationContext(dict):
    def __init__(self, code, context, session_data, **extra):

        self.code = code
        self.context = context
        self.session_data = session_data

        super(EvaluationContext, self).__init__(
            __loader__=self.get_code,
            __traceback_hidden_variables__=HIDDEN_VARIABLES,
        )

    def get_code(self, *args, **opts):
        return self.code

    def __getitem__(self, k):
        for d in (self, self.context, self.session_data):
            try:
                return dict.__getitem__(d, k)
            except KeyError:
                pass

        raise KeyError(k)

    def __setitem__(self, k, v):
        for d in (self.context, self.session_data):
            dict.__setitem__(d, k, v)

    def __iter__(self):
        return iter(self.keys())

    def keys(self):
        return frozenset(
            chain.from_iterable(
                dict.keys(d) for d in (self, self.context, self.session_data)))

    def values(self):
        return (self[k] for k in self)

    def items(self):
        return ((k, self[k]) for k in self)

    def __repr__(self):
        return '<%s>' % self.__class__.__name__


def execute_from_file(path, *args, **opts):
    with open(path, 'r') as f:
        return execute_from_string(force_text(f.read()), *args, **opts)


def execute_from_string(code, globals={}, context={}, session_data={}, **opts):

    __traceback_hidden_variables__ = True

    #this is creating a custom __loader__ that is returning the source code
    #traceback serializers is inspecting global variables and looking for a standard loader that can return source code.

    context = EvaluationContext(
        code, context=context, session_data=session_data)
    result = None
    expressions = list(ast.parse(code).body)

    if not expressions:
        return

    if isinstance(last(expressions), ast.Expr):
        result = expressions.pop(-1)

    if expressions:
        exec(compile(ast.Module(expressions), '', 'exec'), context)

    if result:
        return eval(compile(ast.Expression(result.value), '', 'eval'), context)


class SideEffectSender(logging.Handler):
    def emit(self, record):
        if isinstance(sys.stdout, StdoutProxy):
            sys.stdout.send_side_effect(record.msg)


class SocketWriter:
    def __init__(self, socket):
        self.socket = socket

    def write(self, bytes):
        self.socket.send(bytes)


class StdoutProxy:

    keep_listening = wl.ExternalEvaluate.Private.ExternalEvaluateKeepListening

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


def evaluate_message(input=None, return_type=None, args=None, **opts):

    __traceback_hidden_variables__ = True

    if isinstance(input, six.string_types):
        result = execute_from_string(input, **opts)

    if isinstance(args, (list, tuple)):
        #then we have a function call to do
        #first get the function object we need to call
        result = result(*args)

    if return_type == 'string':
        # bug 354267 repr returns a 'str' even on py2 (i.e. bytes).
        result = force_text(repr(result))

    return result


@to_wl(**EXPORT_KWARGS)
def handle_message(socket):

    __traceback_hidden_variables__ = True

    message = binary_deserialize(socket.recv())
    result = evaluate_message(**message)

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

    side_effect_logger.addHandler(SideEffectSender())

    #now sit in a while loop, evaluating input
    while messages < message_limit:
        stream.write(handle_message(socket))
        messages += 1

    if redirect_stdout:
        sys.stdout = sys.__stdout__
