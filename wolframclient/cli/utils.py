from __future__ import absolute_import, print_function, unicode_literals

import argparse
import os
import sys

from wolframclient.utils.decorators import to_dict
from wolframclient.utils.importutils import module_path
from wolframclient.utils.require import require_module

if hasattr(os, "scandir"):
    # python2 do not support scan which is way faster
    # the function was introduced in py3.5, so it's better to just check if the function is there using hasattr.

    def _scan(folder):
        for f in os.scandir(folder):
            yield f.is_dir(), f.name


else:

    def _scan(folder):
        for f in os.listdir(folder):
            yield os.path.isdir(os.path.join(folder, f)), f


def _discover(module, folder=None, walk=True):
    folder = folder or module_path(module)
    for is_folder, filename in _scan(folder):
        if not is_folder:
            yield module, filename
        elif walk and not filename == "__pycache__":
            for args in _discover(
                "%s.%s" % (module, filename), folder=os.path.join(folder, filename), walk=walk
            ):
                yield args


@to_dict
def discover_with_convention(modules, import_name, walk=True):
    for module in modules:
        for module, filename in _discover(module, walk=walk):
            basename, ext = os.path.splitext(filename)
            if ext == ".py" and not basename == "__init__":
                yield basename, "%s.%s.%s" % (module, basename, import_name)


class SimpleCommand(object):

    help = None
    print = print

    dependencies = ()

    def __init__(self, argv=None, name=None):
        if argv is None:
            self.argv = tuple(sys.argv)
        else:
            self.argv = argv

        self.name = name

    def create_parser(self):
        return argparse.ArgumentParser(prog=self.name, description=self.help)

    def add_arguments(self, parser):
        pass

    def handle(self, *args, **opts):
        pass

    def main(self):

        if self.dependencies:
            require_module(*self.dependencies)

        parser = self.create_parser()
        if parser:
            self.add_arguments(parser)

            cmd_options = vars(parser.parse_args(self.argv[1:]))
            args = cmd_options.pop("args", ())
            return self.handle(*args, **cmd_options)

        return self.handle()
