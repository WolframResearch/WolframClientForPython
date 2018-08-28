# -*- coding: utf-8 -*-
from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl

# Change the path to point to a local kernel. 
# This value corresponds to the default path to a MacOS Wolfram Desktop kernel.
kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'
with WolframLanguageSession(kernel_path) as session:
    # define a matrix of integers
    array = [
        [1, 2, 3], 
        [0, 4, 5], 
        [0, 0, 6]]
    res = session.evaluate(wl.Eigenvalues(array))
    print(res.get())  # [6, 4, 1]
