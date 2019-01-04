# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

if __name__ == '__main__':

    #this will perform an auto install of missing modules using PIP
    #this should not be used in production, but it's handy when we are giving this paclet to other developers
    #as it provides convenient access to unit tests, profiler, and benchmarking.

    from wolframclient.cli.dispatch import execute_from_command_line

    execute_from_command_line()