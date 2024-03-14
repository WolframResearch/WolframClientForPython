from __future__ import absolute_import, print_function, unicode_literals

import os
import subprocess
import sys
from functools import partial

from wolframclient.cli.utils import SimpleCommand
from wolframclient.utils.decorators import to_tuple
from wolframclient.utils.functional import flatten, iterate
from wolframclient.utils.importutils import module_path

process_verbose = partial(
    subprocess.Popen,
    stdout=sys.stdout,
    stdin=subprocess.PIPE,
    stderr=sys.stderr,
    env=os.environ,
)


@to_tuple
def wait_for_process(processes, raise_errors=True, show_output=False):
    for p in iterate(processes):
        if raise_errors and not p.wait() == 0:
            raise ValueError("Process finished with non zero status:")

        p.wait()
        yield p


class Command(SimpleCommand):

    modules = ["wolframclient"]

    @to_tuple
    def _process_args(self, repo, pre, *args):
        yield sys.executable
        yield "-m"
        yield from iterate(pre)
        yield module_path(repo)
        yield from args

    def run(self, pre, *args):
        args = tuple(flatten(args))
        args = tuple(self._process_args(repo, pre, *args) for repo in self.modules)

        for a in args:
            print(" ".join(a))

        return wait_for_process(map(process_verbose, args), raise_errors=False)

    def handle(self, **opts):
        # self.run(("ruff", "format"), "--target-version", "py311")
        # to do import replacement do python -m isort Git/rotostampa --force-single-line-imports

        self.run(
            ("ruff", "check"),
            "--fix",
            "--unsafe-fixes",
            "--select",
            "ALL",
            (
                ("--ignore", r)
                for r in (
                    "ANN001",
                    "ANN002",
                    "ANN003",
                    "ANN101",
                    "ANN102",
                    "ANN201",
                    "ANN202",
                    "ANN204",
                    "ANN205",
                    "ANN206",
                    "ANN401",
                    "B006",
                    "COM812",
                    "D100",
                    "D101",
                    "D102",
                    "D103",
                    "D104",
                    "D105",
                    "D106",
                    "D107",
                    "D200",
                    "D201",
                    "D202",
                    "D203",
                    "D204",
                    "D205",
                    "D206",
                    "D207",
                    "D208",
                    "D209",
                    "D210",
                    "D211",
                    "D212",
                    "D213",
                    "D214",
                    "D215",
                    "D300",
                    "D301",
                    "D400",
                    "D401",
                    "D402",
                    "D403",
                    "D404",
                    "D405",
                    "D406",
                    "D407",
                    "D408",
                    "D409",
                    "D410",
                    "D411",
                    "D412",
                    "D413",
                    "D414",
                    "D415",
                    "D416",
                    "D417",
                    "D418",
                    "D419",
                    "E501",
                    "E731",
                    "EM101",
                    "EM102",
                    "EM103",
                    "RET502",
                    "RET503",
                    "UP032",
                    "FLY002",
                    "PT009",
                    "SIM118",
                    "PT027",
                    "T201",
                    "T203",
                    "UP010",
                    "SIM105",
                    "UP009",
                    "G010",
                    "LOG009",
                )
            ),
        )
        self.run(
            "black",
            "--line-length",
            "95",
            "--target-version",
            "py311",
            "--skip-magic-trailing-comma",
        )
