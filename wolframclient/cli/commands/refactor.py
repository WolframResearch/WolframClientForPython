# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import sys

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils.importutils import module_path


class Command(SimpleCommand):

    modules = ["wolframclient"]

    dependencies = (('isort', "4.3.20"), ("autoflake", "1.3"), ("black", "19.3b0"))

    def _module_args(self, *args):

        yield __file__  # autopep main is dropping the first argument

        for module in self.modules:
            yield module_path(module)

        for arg in args:
            yield arg

    def handle(self, **opts):

        argv = sys.argv

        from autoflake import main

        sys.argv = list(
            self._module_args(
                "--in-place",
                "--remove-duplicate-keys",
                "--expand-star-import",
                "--remove-all-unused-imports",
                "--recursive",
            )
        )

        main()

        from isort.main import main

        sys.argv = list(
            self._module_args(
                '-rc', '--multi-line', '5', '-a',
                "from __future__ import absolute_import, print_function, unicode_literals"
            ))

        main()

        from black import main

        sys.argv = list(self._module_args("--line-length", "95", "--target-version", "py35"))

        main()

        sys.argv = argv
