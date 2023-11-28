from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
from wolframclient.utils.api import os


def installation_version():

    v = os.environ.get("WOLFRAM_KERNEL_VERSION", None)
    if v:
        return float(v)

    return 12.0


def _explore_paths(*paths):
    highest_version = -1
    best_path = None
    for root in paths:
        if os.isdir(root):
            for version in os.listdir(root):
                full_path = os.path_join(root, version)
                if os.isdir(full_path):
                    try:
                        v_num = float(version)
                    except ValueError:
                        v_num = -2
                    if v_num > highest_version and v_num > 0:
                        highest_version = v_num
                        best_path = full_path
    if best_path:
        yield best_path


def _installation_directories():
    env = os.environ.get("WOLFRAM_INSTALLATION_DIRECTORY", None)
    if env:
        yield env

    if six.WINDOWS:
        for p in _explore_paths(
            "C:\\Program Files\\Wolfram Research\\Wolfram Desktop",
            "C:\\Program Files\\Wolfram Research\\Mathematica",
            "C:\\Program Files\\Wolfram Research\\Wolfram Engine",
        ):
            yield p

    elif six.LINUX:
        for p in _explore_paths(
            "/usr/local/Wolfram/Desktop",
            "/usr/local/Wolfram/Mathematica",
            "/usr/local/Wolfram/WolframEngine",
        ):
            yield p

    elif six.MACOS:
        yield "/Applications/Wolfram Desktop.app/Contents"
        yield "/Applications/Mathematica.app/Contents"
        yield "/Applications/Wolfram Engine.app/Contents"


def find_default_kernel_path():
    """ Look for the most recent installed kernel. """

    if six.WINDOWS:
        rel = "WolframKernel.exe"
    elif six.LINUX:
        rel = "Executables/WolframKernel"
    elif six.MACOS:
        rel = "MacOS/WolframKernel"

    for path in _installation_directories():
        if rel:
            path = os.path_join(path, rel)
        if os.isfile(path):
            return path
