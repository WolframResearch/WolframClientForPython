from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl

with WolframLanguageSession() as session:
    # define a matrix of integers
    array = [
        [-1, 1, 1], 
        [1, -1, 1], 
        [1, 1, -1]]
    # expression to evaluate
    expr = wl.Eigenvalues(array)
    # send expression to the kernel for evaluation.
    res = session.evaluate(expr)
    print(res)  # [-2, -2, 1]
