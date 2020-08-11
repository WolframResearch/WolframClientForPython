from __future__ import absolute_import, print_function, unicode_literals

import sys

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils.importutils import module_path, safe_import_string_and_call


class Command(SimpleCommand):

    modules = ["wolframclient"]

    dependencies = (("isort", "5.3.2"), ("autoflake", "1.3"), ("black", "19.3b0"))

    def _module_args(self, *args):

        yield __file__  # autopep main is dropping the first argument

        for module in self.modules:
            yield module_path(module)

        for arg in args:
            yield arg

    def run(self, path, *args):
        originals = sys.argv
        sys.argv = list(self._module_args(*args))
        safe_import_string_and_call(path)
        sys.argv = originals

    def handle(self, **opts):

        # autoflake is not removing imports if they are in a list
        # to fix this we first refactor the code using imports in single line

        self.run(
            "isort.main.main",
            "-rc",
            "-sl",
            "-a",
            "from __future__ import absolute_import, print_function, unicode_literals",
        )

        # then we remove all missing imports and we expand star imports

        self.run(
            "autoflake.main",
            "--in-place",
            "--remove-duplicate-keys",
            "--expand-star-import",
            "--remove-all-unused-imports",
            "--recursive",
        )

        # then we use refactor imports again using pretty newline style

        self.run("isort.main.main", "-rc", "--multi-line", "5")

        # after that we finally run black to refactor all code

        self.run("black.main", "--line-length", "95", "--target-version", "py34")
