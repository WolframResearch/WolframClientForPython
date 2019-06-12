# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import sys
from contextlib import contextmanager

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils.importutils import module_path, safe_import_string_and_call


@contextmanager
def changed_args(args):
    # Code to acquire resource, e.g.:
    originals = sys.argv

    sys.argv = list(args)

    yield

    sys.argv = originals


def run(path, args):
    with changed_args(args):
        safe_import_string_and_call(path)


class Command(SimpleCommand):

    modules = ["wolframclient"]

    dependencies = (("isort", "4.3.20"), ("autoflake", "1.3"), ("black", "19.3b0"))

    def _module_args(self, *args):

        yield __file__  # autopep main is dropping the first argument

        for module in self.modules:
            yield module_path(module)

        for arg in args:
            yield arg

    def handle(self, **opts):

        # autoflake is not removing imports if they are in a list
        # to fix this we first refactor the code using imports in single line

        run(
            "isort.main.main",
            self._module_args(
                "-rc",
                "-sl",
                "-a",
                "from __future__ import absolute_import, print_function, unicode_literals",
            ),
        )

        # then we remove all missing imports and we expand star imports

        run(
            "autoflake.main",
            self._module_args(
                "--in-place",
                "--remove-duplicate-keys",
                "--expand-star-import",
                "--remove-all-unused-imports",
                "--recursive",
            ),
        )

        # then we use refactor imports again using pretty newline style

        run("isort.main.main", self._module_args("-rc", "--multi-line", "5"))

        # after that we finally run black to refactor all code

        run("black.main", self._module_args("--line-length", "95", "--target-version", "py34"))
