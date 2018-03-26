# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.decorators import to_dict, to_tuple

import ast

@to_dict
def string_to_compiled_code(string):
    #this can give a syntax erro

    parsed = ast.parse(string)

    for body in parsed.body:
        yield compile(ast.Expression(body.value), '', 'eval'), string

@to_tuple
def ast_eval(string):
    compiled_code = string_to_compiled_code(string)
    try:
        for code in compiled_code:
            yield eval(code)
    except Exception as e:
        e._compiled_code = compiled_code
        raise