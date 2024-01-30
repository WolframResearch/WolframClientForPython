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
from wolframclient.utils.functional import first, iterate, last

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
        raise TypeError


DEFAULT_HIDDEN_VARIABLES = (
    "__loader__",
    "__builtins__",
    "__traceback_hidden_variables__",
    "absolute_import",
    "print_function",
    "unicode_literals",
)


class registry(dict):
    def __repr__(self):
        return "<registry len=%s>" % len(self)


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


def to_external_object(instance, objects_registry):
    pk = id(instance)
    objects_registry[pk] = instance

    # meta = dict(_serialize_external_object_meta(instance))
    meta = {}
    func = callable(instance) and wl.ExternalFunction or wl.ExternalObject

    return func(wl.Inherited, pk, meta)


def object_processor(serializer, instance, objects_registry):
    return serializer.encode(to_external_object(instance, objects_registry))


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


def EvaluationEnvironment(code, globals_registry, constants=None, **extra):
    globals_registry["__loader__"] = Settings(get_source=lambda module, code=code: code)
    globals_registry["__traceback_hidden_variables__"] = DEFAULT_HIDDEN_VARIABLES
    if constants:
        globals_registry.update(constants)

    return globals_registry


# ROUTES DEFINITION, we declare a global registry and series of functions

def check_wl_symbol(expr, symbol):
    return (
        isinstance(expr, WLFunction)
        and isinstance(expr.head, WLSymbol)
        and expr.head == symbol
    )

def unpack_optionals(args, symbol = wl.Rule):
    
    positional = []
    optionals = {}

    for expr in args:
        if check_wl_symbol(expr, symbol):
            optionals[expr[0]] = expr[1]
        else:
            positional.append(expr)

    return positional, optionals


BUILTIN_ROUTES = registry()


def register_route(func):
    BUILTIN_ROUTES[func.__name__] = func
    return func


@register_route
def Eval(consumer, code):
    # this is creating a custom __loader__ that is returning the source code
    # traceback serializers is inspecting global variables and looking for a standard loader that can return source code.

    env = EvaluationEnvironment(code=code, globals_registry=consumer.globals_registry)
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


@register_route
def Fetch(consumer, input):
    try:
        return consumer.objects_registry[input]
    except KeyError:
        raise KeyError("Object with id %s cannot be found in this session" % input)


@register_route
def Set(consumer, value, *names):
    for name in names:
        assert isinstance(name, six.string_types)
        consumer.globals_registry[name] = value
    return value


@register_route
def Effect(consumer, *args):
    return last(args)


@register_route
def Call(consumer, result, *args):

    pos, kwargs = unpack_optionals(args)

    return result(*pos, **kwargs)


@register_route
def MethodCall(consumer, result, names, *args):
    return Call(consumer, GetAttribute(consumer, result, names), *args)


@register_route
def Curry(consumer, result, *args):

    pos, kwargs = unpack_optionals(args)

    return partial(result, *pos, **kwargs)


@register_route
def ReturnType(consumer, result, return_type):
    if return_type == "String":
        # bug 354267 repr returns a 'str' even on py2 (i.e. bytes).
        return force_text(repr(result))
    elif return_type == "ExternalObject":
        return to_external_object(result, consumer.objects_registry)
    elif return_type != "Expression":
        raise NotImplementedError("Return type %s is not implemented" % return_type)

    return result


@register_route
def GetAttribute(consumer, result, names):
    for name in iterate(names):
        result = getattr(result, name)
    return result


@register_route
def GetItem(consumer, result, names):
    for name in iterate(names):
        result = result[name]
    return result


@register_route
def SetAttribute(consumer, result, name, value):
    setattr(result, name, value)
    return result


@register_route
def SetItem(consumer, result, name, value):
    result[name] = value
    return result


@register_route
def Length(consumer, result):
    return len(result)


class ExternalEvaluateConsumer(WXFConsumerNumpy):
    hook_symbol = wl.ExternalEvaluate.Private.ExternalEvaluateCommand

    builtin_routes = BUILTIN_ROUTES

    def __init__(self, routes_registry={}):
        self.objects_registry = registry()
        self.globals_registry = registry()
        self.routes_registry = registry(routes_registry, **self.builtin_routes)

    def consume_function(self, *args, **kwargs):
        expr = super().consume_function(*args, **kwargs)

        if check_wl_symbol(expr, self.hook_symbol):
            assert len(expr.args) == 2
            return self.dispatch_wl_object(*expr.args)

        return expr

    def dispatch_wl_object(self, route, args):
        return self.routes_registry[route](self, *args)

    def __repr__(self):
        return "<{} globals={} objects={}>".format(
            self.__class__.__name__, len(self.globals_registry), len(self.objects_registry)
        )


class SocketWriter:
    keep_listening = wl.ExternalEvaluate.Private.ExternalEvaluateKeepListening

    def __init__(self, socket):
        self.socket = socket

    def write(self, bytes):
        self.socket.send(zmq.Frame(bytes))

    def send_side_effect(self, expr):
        self.write(export(self.keep_listening(expr), target_format="wxf"))


def handle_message(socket, consumer):
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
    message_limit=float("inf"), exception_class=None, evaluate_message=None, **opts
):
    consumer = ExternalEvaluateConsumer(
        routes_registry=evaluate_message and {"Eval": evaluate_message} or {}
    )

    handler = to_wl(
        exception_class=exception_class,
        object_processor=lambda serializer, instance: serializer.encode(
            to_external_object(instance, consumer.objects_registry)
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
