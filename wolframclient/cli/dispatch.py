from __future__ import absolute_import, print_function, unicode_literals

import sys

from wolframclient.cli.utils import SimpleCommand, discover_with_convention
from wolframclient.utils.importutils import import_string
from wolframclient.utils.require import require_module


class DispatchCommand(SimpleCommand):

    modules = ["wolframclient.cli.commands"]
    class_name = "Command"

    default_command = None

    dependencies = ()

    def subcommands(self):
        return discover_with_convention(self.modules, self.class_name)

    def handle(self, attr=None):

        all_commands = self.subcommands()

        if attr is None and self.default_command:
            attr = self.default_command

        if attr in all_commands:
            return import_string(all_commands[attr])(
                self.subcommand_args(), name=all_commands[attr]
            ).main()

        self.print("Select one of the following commands:")
        for command in sorted(all_commands.keys()):
            self.print(" -", command)

        sys.exit(1)

    def subcommand_args(self):
        argv = list(self.argv)
        if len(argv) > 1:
            argv.pop(1)
        return argv

    def main(self):

        if self.dependencies:
            require_module(*self.dependencies)

        if len(self.argv) > 1 and self.argv[1]:
            return self.handle(self.argv[1])
        return self.handle()


def execute_from_command_line(argv=None, **opts):
    return DispatchCommand(argv).main()
