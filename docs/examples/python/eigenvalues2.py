# -*- coding: utf-8 -*-
from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl
from wolframclient.deserializers import WXFConsumer, binary_deserialize

# TODO: Change this value with a valid local path to a Wolfram Kernel.
kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'


class ComplexFunctionConsumer(WXFConsumer):
    """Implement a consumer that maps Complex to python complex types."""
    
    # represent the symbol Complex as a Python class
    Complex = wl.Complex

    def build_function(self, head, args, **kwargs):
        # return a built in complex if head is Complex and argument length is 2.
        if head == self.Complex and len(args) == 2:
            return complex(*args)
        # otherwise delegate the super method (default case).
        else:
            return super().build_function(head, args, **kwargs)

with WolframLanguageSession(kernel_path) as session:
    array = [
        [0, -2, 0], 
        [1, 0, -1], 
        [0, 2, 0]]
    # standard evaluation uses the default WXF parser:
    res = session.Eigenvalues(complex_array))
    print(res)  # [Complex[ << 2 >> ], 0.0]
    # alternativelly, get the WXF bytes and parse them using the complex consumer:
    complex_result = binary_deserialize(res.wxf, consumer=ComplexFunctionConsumer())
    print(complex_result)  # [(1+2j), 0.0]
