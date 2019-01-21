from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl
from wolframclient.deserializers import WXFConsumer, binary_deserialize

# represent the symbol Complex as a Python class
Complex = wl.Complex

class ComplexFunctionConsumer(WXFConsumer):
    """Implement a consumer that maps Complex to python complex types."""
    def build_function(self, head, args, **kwargs):
        if head == Complex and len(args) == 2:
            return complex(*args)
        else:
            return super().build_function(head, args, **kwargs)

with WolframLanguageSession() as session:
    array = [
        [wl.Pi, -2, 0], 
        [1, wl.Pi, -1],
        [0, 2, wl.Pi]]

    # expression to evaluate: N[EigenValues[array]]
    expr = wl.N(wl.Eigenvalues(array))
    
    # evaluate without deserializing
    wxf = session.evaluate_wxf(expr)
    # deserialize using the math consumer:
    complex_result = binary_deserialize(wxf, consumer=ComplexFunctionConsumer())
    # [(3.141592653589793+2j), (3.141592653589793-2j), 3.141592653589793]
    print(complex_result)