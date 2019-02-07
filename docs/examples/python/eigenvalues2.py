from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl
from wolframclient.deserializers import WXFConsumer, binary_deserialize

class ComplexFunctionConsumer(WXFConsumer):
    """Implement a consumer that maps Complex to python complex types."""
    
    # represent the symbol Complex as a Python class
    Complex = wl.Complex

    def build_function(self, head, args, **kwargs):
        # return a built in complex if head is Complex and argument length is 2.
        if head == self.Complex and len(args) == 2:
            return complex(*args)
        # otherwise delegate to the super method (default case).
        else:
            return super().build_function(head, args, **kwargs)

with WolframLanguageSession() as session:
    array = [
        [0, -2, 0], 
        [1, 0, -1], 
        [0, 2, 0]]
    # expression to evaluate
    expr = wl.Eigenvalues(array)
    # send expression to the kernel for evaluation.
    wxf = session.evaluate_wxf(expr)
    # get the WXF bytes and parse them using the complex consumer:
    complex_result = binary_deserialize(
        wxf, 
        consumer=ComplexFunctionConsumer())
    print(complex_result)  # [2j, -2j, 0]
