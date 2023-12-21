from __future__ import absolute_import, print_function, unicode_literals

import logging
import os
import sys
import inspect
from functools import partial

from wolframclient.deserializers import binary_deserialize
from wolframclient.deserializers.wxf.wxfconsumer import WXFConsumerNumpy
from wolframclient.language import wl
from wolframclient.language.decorators import to_wl
from wolframclient.language.expression import WLFunction, WLSymbol
from wolframclient.language.side_effects import side_effect_logger
from wolframclient.serializers import export
from wolframclient.utils import six
from wolframclient.utils.functional import first
from wolframclient.utils.api import ast, zmq
from wolframclient.utils.datastructures import Settings
from wolframclient.utils.encoding import force_text
from wolframclient.utils.functional import last

"""

The kernel will a WXF input to python.
The message needs to be deserialized however a particular function called ExternalEvaluate`Private`MakePythonObject will be evaluated. The function call looks like this

    MakePythonObject({"mode": "type", "input": "date", "arguments": {2012, 10, 10}})
    MakePythonObject({"mode": "command", "input": "range", "arguments": {0, 10}})
    MakePythonObject({"mode": "command", "input": 1, "arguments": {}})
    MakePythonObject({"mode": "function", "input": 1, "arguments": {}})


Possibile key of the commands are:

## mode

### type

the mode type is used to create a type from argument. Possibile types include date, datetime, fraction, image.

### command

the mode command is used to create a python object, takes an input which is either a string or an integer. the string is python code to be parsed and executed, the integer is a reference to an existing python object in the session.

### function

the mode function is similar to the previous one but is supposed to return a function, so it will always curry the argument spec.

## input

the input of the function call, for type it identifies a function to execute by name, for object and function it is the command to create it.

## arguments

extra arguments to initialize the object or function with

## return_type

the return type of the object creation, 

### expr

expr will keep the result as is.

### string

will run repr over the result

### object

will return a WL ExternalObject or ExternalFunction.

"""


if hasattr(inspect, "getfullargspec"):
    inspect_args = inspect.getfullargspec
elif hasattr(inspect, "getargspec"):
    inspect_args = inspect.getargspec
else:

    def inspect_args(f):
        raise TypeError()



class WXFExternalObjectConsumer(WXFConsumerNumpy):
    def __init__(self, external_object_registry):
        self.external_object_registry = external_object_registry

    def consume_function(self, *args, **kwargs):
        expr = super().consume_function(*args, **kwargs)

        if (
            isinstance(expr, WLFunction)
            and isinstance(expr.head, WLSymbol)
            and (
                expr.head.name == "ExternalObject"
                or expr.head.name == "ExternalFunction"
            )
        ):
            session_id = expr.args[0]["ObjectID"]

            return self.external_object_registry[session_id]

        return expr


def _serialize_external_object_meta(o):

    if callable(o):
        yield "Type", "PythonFunction"
        try:
            # force tuple to avoid calling this method again on `map`.
            yield "Arguments", tuple(map(force_text, first(inspect_args(o))))
        except TypeError:
            # this function can fail with TypeError unsupported callable
            pass
    else:
        yield "Type", "PythonObject"

    is_module = inspect.ismodule(o)

    yield "IsModule", is_module

    if not is_module:
        module = inspect.getmodule(o)
        if module:
            yield "Module", force_text(module.__name__)

    yield "IsClass", inspect.isclass(o),
    yield "IsFunction", inspect.isfunction(o),
    yield "IsMethod", inspect.ismethod(o),
    yield "IsCallable", callable(o),


def external_object_processor(serializer, instance, external_object_registry):
    pk = id(instance)
    external_object_registry[pk] = instance


    cmd = {'Command': id(instance)}
    meta = dict(_serialize_external_object_meta(instance))

    if callable(instance):
        expr = wl.ExternalFunction(wl.Inherited, cmd, meta)
    else:
        expr = wl.ExternalObject(wl.Inherited, cmd, meta)

    return serializer.encode(expr)


HIDDEN_VARIABLES = (
    "__loader__",
    "__builtins__",
    "__traceback_hidden_variables__",
    "absolute_import",
    "print_function",
    "unicode_literals",
)


if six.PY_38:

    # https://bugs.python.org/issue35766
    # https://bugs.python.org/issue35894
    # https://github.com/ipython/ipython/issues/11590
    # PY_38 requires type_ignores to be a list, other versions are not accepting a second argument

    def Module(code, type_ignores=[]):
        return ast.Module(code, type_ignores)


else:

    def Module(code):
        return ast.Module(code)


def EvaluationEnvironment(code, session_data={}, constants=None, **extra):

    session_data["__loader__"] = Settings(get_source=lambda module, code=code: code)
    session_data["__traceback_hidden_variables__"] = HIDDEN_VARIABLES
    if constants:
        session_data.update(constants)

    return session_data


def execute_from_file(path, *args, **opts):
    with open(path, "r") as f:
        return execute_from_string(force_text(f.read()), *args, **opts)


def execute_from_string(code, globals, **opts):

    __traceback_hidden_variables__ = ["env", "current", "__traceback_hidden_variables__"]

    # this is creating a custom __loader__ that is returning the source code
    # traceback serializers is inspecting global variables and looking for a standard loader that can return source code.

    env = EvaluationEnvironment(code=code, **opts)
    result = None
    expressions = list(
        compile(
            code,
            filename="<unknown>",
            mode="exec",
            flags=ast.PyCF_ONLY_AST | unicode_literals.compiler_flag,
        ).body
    )

    if not expressions:
        return

    last_expr = last(expressions)

    if isinstance(last_expr, ast.Expr):
        result = expressions.pop(-1)

    if expressions:
        exec(compile(Module(expressions), "", "exec"), env)

    if result:
        return eval(compile(ast.Expression(result.value), "", "eval"), env)

    elif isinstance(last_expr, (ast.FunctionDef, ast.ClassDef)):
        return env[last_expr.name]


class SocketWriter:

    keep_listening = wl.ExternalEvaluate.Private.ExternalEvaluateKeepListening

    def __init__(self, socket):
        self.socket = socket

    def write(self, bytes):
        self.socket.send(zmq.Frame(bytes))

    def send_side_effect(self, expr):
        self.write(export(self.keep_listening(expr), target_format="wxf"))


def evaluate_message(input=None, return_type=None, args=None, **opts):

    __traceback_hidden_variables__ = True

    result = None

    if isinstance(input, six.string_types):
        result = execute_from_string(input, **opts)

    if isinstance(args, (list, tuple)):
        # then we have a function call to do
        # first get the function object we need to call
        result = result(*args)

    if return_type == "string":
        # bug 354267 repr returns a 'str' even on py2 (i.e. bytes).
        result = force_text(repr(result))

    return result


def handle_message(socket, evaluate_message, consumer):

    __traceback_hidden_variables__ = True

    message = binary_deserialize(socket.recv(copy=False).buffer, consumer=consumer)
    result = evaluate_message(**message)

    sys.stdout.flush()
    return result


def start_zmq_instance(port=None, write_to_stdout=True, **opts):

    # make a reply socket
    sock = zmq.Context.instance().socket(zmq.PAIR)
    # now bind to a open port on localhost
    if port:
        sock.bind("tcp://127.0.0.1:%s" % port)
    else:
        sock.bind_to_random_port("tcp://127.0.0.1")

    if write_to_stdout:
        sys.stdout.write(force_text(sock.getsockopt(zmq.LAST_ENDPOINT)))
        sys.stdout.write(os.linesep)  # writes \n
        sys.stdout.flush()

    return sock


def start_zmq_loop(
    message_limit=float("inf"), evaluate_message=evaluate_message, exception_class=None, **opts
):

    external_object_registry = {}
    evaluate_message = partial(
        evaluate_message, external_object_registry=external_object_registry, globals={}
    )

    consumer = WXFExternalObjectConsumer(external_object_registry)

    handler = to_wl(
        exception_class=exception_class,
        external_object_processor=partial(
            external_object_processor, external_object_registry=external_object_registry
        ),
        target_format="wxf",
    )(handle_message)
    socket = start_zmq_instance(**opts)
    stream = SocketWriter(socket)
    messages = 0

    class SideEffectSender(logging.Handler):
        def emit(self, record):
            stream.send_side_effect(record.msg)

    side_effect_logger.addHandler(SideEffectSender())

    # now sit in a while loop, evaluating input
    while messages < message_limit:
        stream.write(handler(socket, evaluate_message=evaluate_message, consumer=consumer))
        messages += 1
