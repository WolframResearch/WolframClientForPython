# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
from wolframclient.utils.api import os


def explore_paths(*paths):
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


def installation_directories():
    env = os.environ.get('WOLFRAM_INSTALLATION_DIRECTORY', None)
    if env:
        yield env

    if six.WINDOWS:
        for p in explore_paths(
                'C:\\Program Files\\Wolfram Research\\Wolfram Desktop',
                'C:\\Program Files\\Wolfram Research\\Mathematica'):
            yield p

    elif six.LINUX:
        for p in explore_paths('/usr/local/Wolfram/Desktop'
                               '/usr/local/Wolfram/Mathematica'):
            yield p

    elif six.MACOS:
        yield '/Applications/Wolfram Desktop.app/Contents'
        yield '/Applications/Mathematica.app/Contents'


def exe_path():
    if six.WINDOWS:
        return 'WolframKernel.exe'
    elif six.LINUX:
        return 'Executables/WolframKernel'
    elif six.MACOS:
        return 'MacOS/WolframKernel'


def find_default_kernel_path():
    """ Look for the most recent installed kernel. """
    rel = exe_path()
    for path in installation_directories():
        if rel:
            path = os.path_join(path, rel)
        if os.isfile(path):
            return path
