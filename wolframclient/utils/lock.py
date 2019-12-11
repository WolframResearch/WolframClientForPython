from __future__ import absolute_import, print_function, unicode_literals

import warnings

try:
    import multiprocessing

    _lock = multiprocessing.Lock()

    def Lock():
        return _lock


except (ImportError, OSError):

    # JYTHON is raising an ImportError when running "import multiprocessing"
    # GVisor is raising an OSError when running "multiprocessing.Lock()" because the feature is not implemented

    from contextlib import contextmanager

    @contextmanager
    def Lock():
        warnings.warn("Lock is not implemented in the current interpreter.", RuntimeWarning)
        yield
