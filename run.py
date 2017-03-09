# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

if __name__ == '__main__':

    #this will perform an auto install of missing modules using PIP
    #this won't be used in production, but it's handy when we are ginving this paclet to other developers

    from wolframclient.cli.dispatch import execute_from_command_line

    execute_from_command_line()