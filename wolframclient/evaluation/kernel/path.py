# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.utils import six
from wolframclient.utils.api import os

if six.WINDOWS or six.LINUX:
    if six.WINDOWS:
        APP_ROOT_PATH = [
            'C:\\Program Files\\Wolfram Research\\Wolfram Desktop\\',
            'C:\\Program Files\\Wolfram Research\\Mathematica\\',
        ]
        EXE_REL_PATH = 'wolfram.exe'
    elif six.LINUX:
        APP_ROOT_PATH = [
            '/usr/local/Wolfram/Desktop/',
            '/usr/local/Wolfram/Mathematica/',
        ]
        EXE_REL_PATH = '/Files/Executables/wolfram'

    def os_default_kernel_path():
        highest_version = -1
        best_path = None
        for root in APP_ROOT_PATH:
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
            return os.path_join(best_path, EXE_REL_PATH)
        else:
            return None
elif six.MACOS:
    DEFAULT_PATHS = [
        '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel',
        '/Applications/Mathematica.app/Contents/MacOS/WolframKernel',
    ]

    def os_default_kernel_path():
        for path in DEFAULT_PATHS:
            if os.isfile(path):
                return path
        return None
else:

    def os_default_kernel_path():
        return None

def find_default_kernel_path():
    """ Look for the most recent installed kernel. """
    return os.environ.get('WOLFRAM_KERNEL_PATH', None) or os_default_kernel_path()