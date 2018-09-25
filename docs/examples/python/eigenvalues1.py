# -*- coding: utf-8 -*-
from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl

# Change the path to point to a local kernel. 
# This value corresponds to the default path to a MacOS Wolfram Desktop kernel.
kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'

with WolframLanguageSession(kernel_path) as session:
    # define a matrix of integers
    array = [
        [-1, 1, 1], 
        [1, -1, 1], 
        [1, 1, -1]]
    # expression to evaluate
    expr = wl.Eigenvalues(array)
    # send expression to the kernel for evaluation.
    print(res)  # [-2, -2, 1]
