from __future__ import absolute_import, print_function, unicode_literals

import inspect
import logging
import os
import sys
from functools import partial

from wolframclient.deserializers import binary_deserialize
from wolframclient.deserializers.wxf.wxfconsumer import WXFConsumerNumpy
from wolframclient.language import wl
from wolframclient.language.decorators import to_wl
from wolframclient.language.expression import WLFunction, WLSymbol
from wolframclient.language.side_effects import side_effect_logger
from wolframclient.serializers import export
from wolframclient.utils import six
from wolframclient.utils.api import ast, zmq
from wolframclient.utils.datastructures import Settings
from wolframclient.utils.encoding import force_text
from wolframclient.utils.functional import first, last

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

    yield "Repr", repr(o)


def to_external_object(instance, external_object_registry):
    pk = id(instance)
    external_object_registry[pk] = instance

    cmd = {"Command": id(instance)}
    meta = dict(_serialize_external_object_meta(instance))
    func = callable(instance) and wl.ExternalFunction or wl.ExternalObject

    return func(wl.Inherited, cmd, meta)


def object_processor(serializer, instance, external_object_registry):
    pk = id(instance)
    external_object_registry[pk] = instance

    cmd = {"Command": id(instance)}
    meta = dict(_serialize_external_object_meta(instance))
    func = callable(instance) and wl.ExternalFunction or wl.ExternalObject

    return serializer.encode(func(wl.Inherited, cmd, meta))


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


def EvaluationEnvironment(code, session_globals, constants=None, **extra):
    session_globals["__loader__"] = Settings(get_source=lambda module, code=code: code)
    session_globals["__traceback_hidden_variables__"] = HIDDEN_VARIABLES
    if constants:
        session_globals.update(constants)

    return session_globals


def execute_eval(code, session_globals, **opts):
    __traceback_hidden_variables__ = True

    # this is creating a custom __loader__ that is returning the source code
    # traceback serializers is inspecting global variables and looking for a standard loader that can return source code.

    env = EvaluationEnvironment(code=code, session_globals=session_globals, **opts)
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


def execute_from_id(input, external_object_registry, **opts):
    __traceback_hidden_variables__ = True

    try:
        return external_object_registry[input]
    except KeyError:
        raise KeyError("Object with id %s cannot be found in this session" % input)



def execute_set(name, value, external_object_registry, **extra):

    assert isinstance(name, six.string_types)

    external_object_registry[name] = value

    return value

def execute_effect(*args, **extra):
    return last(args)

def execute_call(result, args, **extra):
    return result(*args)

def execute_return_rype(result, return_type, **extra):
    if return_type == "String":
        # bug 354267 repr returns a 'str' even on py2 (i.e. bytes).
        return force_text(repr(result))
    elif return_type == "ExternalObject":
        return to_external_object(result, external_object_registry)
    elif return_type != "Expression":
        raise NotImplementedError("Return type %s is not implemented" % return_type)

    return result

def dispatch_wl_object(name, args, dispatch_routes, **extra):
    return dispatch_routes[name](*args, **extra)


class WXFNestedObjectConsumer(WXFConsumerNumpy):
    hook_symbol = wl.ExternalEvaluate.Private.ExternalEvaluateCommand

    builtin_routes = {
        'Set': execute_set,
        'Effect': execute_effect,
        'Eval': execute_eval,
        'Call': execute_call,
        'ReturnType': execute_return_rype
    }

    def __init__(self, external_object_registry, session_globals, dispatch_routes):
        self.external_object_registry = external_object_registry
        self.session_globals = session_globals
        self.dispatch_routes = dict(dispatch_routes, **self.builtin_routes)

    def consume_function(self, *args, **kwargs):
        expr = super().consume_function(*args, **kwargs)

        if (
            isinstance(expr, WLFunction)
            and isinstance(expr.head, WLSymbol)
            and expr.head == self.hook_symbol
        ):
            assert len(expr.args) == 2
            return dispatch_wl_object(
                *expr.args,
                dispatch_routes=self.dispatch_routes,
                session_globals=self.session_globals,
                external_object_registry=self.external_object_registry
            )

        return expr


class SocketWriter:
    keep_listening = wl.ExternalEvaluate.Private.ExternalEvaluateKeepListening

    def __init__(self, socket):
        self.socket = socket

    def write(self, bytes):
        self.socket.send(zmq.Frame(bytes))

    def send_side_effect(self, expr):
        self.write(export(self.keep_listening(expr), target_format="wxf"))


def handle_message(socket, consumer):
    __traceback_hidden_variables__ = True

    result = binary_deserialize(socket.recv(copy=False).buffer, consumer=consumer)

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
    message_limit=float("inf"), exception_class=None, evaluate_message=execute_eval, **opts
):
    external_object_registry = {}
    session_globals = {}

    consumer = WXFNestedObjectConsumer(
        external_object_registry=external_object_registry,
        session_globals=session_globals,
        dispatch_routes={"Eval": evaluate_message},
    )

    handler = to_wl(
        exception_class=exception_class,
        object_processor=lambda serializer, instance, external_object_registry=external_object_registry: serializer.encode(
            to_external_object(instance, external_object_registry)
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
        stream.write(handler(socket, consumer=consumer))
        messages += 1
