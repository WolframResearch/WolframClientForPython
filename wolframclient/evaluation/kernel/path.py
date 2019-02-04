# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
from wolframclient.utils.api import os

def installation_directories():

    env = os.environ.get('WOLFRAM_INSTALLATION_DIRECTORY', None)
    if env:
        yield env

    if six.WINDOWS:
        yield 'C:\\Program Files\\Wolfram Research\\Wolfram Desktop\\'
        yield 'C:\\Program Files\\Wolfram Research\\Mathematica\\'
    elif six.LINUX:
        yield '/usr/local/Wolfram/Desktop/'
        yield '/usr/local/Wolfram/Mathematica/'
    elif six.MACOS:
        yield '/Applications/Wolfram Desktop.app/'
        yield '/Applications/Mathematica.app/'

def exe_path():
    if six.WINDOWS:
        return 'wolfram.exe'
    elif six.LINUX:
        return 'Files/Executables/wolfram'
    elif six.MACOS:
        return 'Contents/MacOS/WolframKernel'

if six.WINDOWS or six.LINUX:
    def find_default_kernel_path():
        highest_version = -1
        best_path = None
        for root in installation_directories():
            if os.isdir(root):
                for version in os.listdir(root):
                    full_path = os.path_join(root, version)
                    if os.isdir(full_path):
                        try:
                            v_num = float(version)
                        except ValueError:
                            continue
                        if v_num > highest_version:
                            highest_version = v_num
                            best_path = full_path
        if highest_version > 0:
            return os.path_join(best_path, exe_path())
        else:
            return None
else:
    def find_default_kernel_path():
        for root in installation_directories():
            path = os.path.join(root, exe_path())
            if os.isfile(path):
                return path
        return None

find_default_kernel_path.__doc__ = """ Look for the most recent installed kernel. """