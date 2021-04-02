from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.importutils import safe_import_string_and_call

try:
    _lock = safe_import_string_and_call("multiprocessing.Lock")

    def Lock():
        return _lock


except (ImportError, OSError):

    # JYTHON is raising an ImportError when running "import multiprocessing"
    # GVisor is raising an OSError when running "multiprocessing.Lock()" because the feature is not implemented

    import warnings
    from contextlib import contextmanager

    warnings.warn("Lock is not implemented in the current interpreter.", RuntimeWarning)

    @contextmanager
    def Lock():
        yield
