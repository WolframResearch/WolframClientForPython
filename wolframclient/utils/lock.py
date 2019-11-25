from __future__ import absolute_import, print_function, unicode_literals

from contextlib import contextmanager

try:
    import multithreading

    _lock = multithreading.Lock()

    @contextmanager
    def Lock():
        with _lock:
            yield


except (ImportError, OSError):

    #JYTHON is raising an ImportError when running "import multithreading"
    #GVisor is raising an OSError when running "multithreading.Lock()" because the feature is not implemented

    @contextmanager
    def Lock():
        yield
