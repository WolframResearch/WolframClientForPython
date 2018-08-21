# -*- coding: utf-8 -*-

from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl

wk = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'
with WolframLanguageSession(wk, kernel_loglevel=logging.DEBUG) as session:
    res = session.evaluate(wl.Transpose([[0,1],[-1,0]]))
    print(res.get())
