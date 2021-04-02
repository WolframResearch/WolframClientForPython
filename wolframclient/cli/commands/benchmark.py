from __future__ import absolute_import, print_function, unicode_literals

import decimal
import os
import tempfile

from wolframclient.cli.utils import SimpleCommand
from wolframclient.deserializers import binary_deserialize
from wolframclient.language import wl
from wolframclient.language.array import NumericArray, PackedArray
from wolframclient.serializers import export
from wolframclient.utils.debug import timed
from wolframclient.utils.encoding import force_text
from wolframclient.utils.functional import first
from wolframclient.utils.importutils import safe_import_string_and_call


def repeat(el, n=1):
    return tuple(el for _ in range(n))


class Command(SimpleCommand):

    col_size = 8
    title_size = 14
    repetitions = 10
    complexity = [1, 2, 5, 10, 100, 1000]

    def add_arguments(self, parser):
        parser.add_argument("--profile", dest="profile", default=False, action="store_true")

    def expression_handler(self, complexity):
        return {
            "expr": {
                "symbols": repeat(wl.Symbol, complexity),
                "strings": repeat("string", complexity),
                "bytes": repeat(b"bytes", complexity),
                "integers": repeat(1, complexity),
                "decimals": repeat(decimal.Decimal("1.23"), complexity),
                "floats": repeat(1.23, complexity),
                "dict": repeat({1: 2, 3: 4, 5: 6}, complexity),
                "list": repeat([1, 2, 3], complexity),
                "functions": repeat(wl.Function(1, 2, 3), complexity),
            },
            "array": {
                "%s_%s" % (func.__name__, t): func(tuple(range(complexity * 100)), t)
                for func in (PackedArray, NumericArray)
                for t in ("Integer64", "Real64")
            },
        }

    def formatted_time(self, function, *args, **opts):

        time = sum(first(timed(function)(*args, **opts)) for i in range(self.repetitions))

        return "%.5f" % (time / self.repetitions)

    def table_line(self, *iterable):
        self.print(
            *(
                force_text(c).ljust(i and self.col_size or self.title_size)
                for i, c in enumerate(iterable)
            )
        )

    def table_divider(self, length):
        self.print(*("-" * (i and self.col_size or self.title_size) for i in range(length)))

    def stream_generators(self, path):
        yield "Memory", lambda complexity, export_format, path=path: None
        yield "File", lambda complexity, export_format, path=path: os.path.join(
            path, "benchmark-test-%s.%s" % (force_text(complexity).zfill(7), export_format)
        )

    def report(self):

        path = tempfile.gettempdir()

        benchmarks = [(c, self.expression_handler(c)) for c in self.complexity]

        self.table_line("dumping results in %s" % path)
        self.table_line()

        # running export to do all lazy loadings
        binary_deserialize(export(1, target_format="wxf"))

        self.table_line("* Binary deserialize")
        self.table_line()

        self.table_line(
            "Memory", *(force_text(c).ljust(self.col_size) for c in self.complexity)
        )
        self.table_divider(len(self.complexity) + 1)

        for label, opts in (("wxf", dict()), ("wxf zip", dict(compress=True))):

            self.table_line(
                label,
                *(
                    self.formatted_time(
                        binary_deserialize, export(expr, target_format="wxf", **opts)
                    )
                    for complexity, expr in benchmarks
                )
            )

        self.table_line()

        self.table_line("* Export")
        self.table_line()

        for title, stream_generator in self.stream_generators(path):

            self.table_line(
                title, *(force_text(c).ljust(self.col_size) for c in self.complexity)
            )
            self.table_divider(len(self.complexity) + 1)

            for key in ("expr", "array"):
                for label, export_format, opts in (
                    ("wl", "wl", dict()),
                    ("wxf", "wxf", dict()),
                    ("wxf zip", "wxf", dict(compress=True)),
                ):
                    if key == "expr" or (key == "array" and not label == "wl"):
                        self.table_line(
                            key == "expr" and label or "%s %s" % (label, key),
                            *(
                                self.formatted_time(
                                    export,
                                    expr[key],
                                    stream=stream_generator(complexity, export_format),
                                    target_format=export_format,
                                    **opts
                                )
                                for complexity, expr in benchmarks
                            )
                        )

            self.table_line()

    def handle(self, profile, **opts):
        if profile:
            safe_import_string_and_call(
                "cProfile.runctx", "report()", {"report": self.report}, {}
            )
        else:
            self.report()
