# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.datastructures import Settings
from wolframclient.utils.decorators import to_dict, to_tuple

import ast

def string_to_compiled_code(string):
    #this can give a syntax erro

    for body in ast.parse(string).body:
        yield compile(ast.Expression(body.value), '', 'eval')

@to_tuple
def ast_eval(string, context = {}):

    #this is creating a custom __loader__ that is returning the source code
    #traceback serializers is inspecting global variables and looking for a standard loader that can return source code.

    loader = Settings(get_source = lambda module, code = string: code)

    for code in string_to_compiled_code(string):
        yield eval(code, dict({'__loader__': loader}, **context))