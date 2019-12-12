from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils.importutils import safe_import_string_and_call

try:

    _lock = safe_import_string_and_call('multithreading.Lock')
    def Lock():
        return _lock

except (ImportError, OSError):

    # JYTHON is raising an ImportError when running "import multithreading"
    # GVisor is raising an OSError when running "multithreading.Lock()" because the feature is not implemented

    from contextlib import contextmanager
    import warnings

    @contextmanager
    def Lock():
        warnings.warn("Lock is not implemented in the current interpreter.", RuntimeWarning)
        yield
