# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

import sys

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils.importutils import module_path


class Command(SimpleCommand):

    modules = ['wolframclient']

    dependencies = (('autopep8', '1.4'), ('isort', '4.3.4'),
                    ('yapf', '0.24.0'), ('autoflake', '1.2'))

    def _module_args(self, *args):

        yield __file__  # autopep main is dropping the first argument

        for module in self.modules:
            yield module_path(module)

        for arg in args:
            yield arg

    def handle(self, **opts):

        argv = sys.argv

        from autoflake import main

        sys.argv = tuple(
            self._module_args('--in-place', '--remove-duplicate-keys',
                              '--expand-star-import',
                              '--remove-all-unused-imports', '--recursive'))

        main()

        from isort.main import main

        sys.argv = list(
            self._module_args(
                '-rc', '--multi-line', '5', '-a',
                "from __future__ import absolute_import, print_function, unicode_literals"
            ))

        main()

        import yapf

        sys.argv = list(
            self._module_args('--in-place', '--recursive', '--parallel'))

        yapf.run_main()

        sys.argv = argv
