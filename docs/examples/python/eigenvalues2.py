# -*- coding: utf-8 -*-
from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl
from wolframclient.deserializers import WXFConsumer, binary_deserialize

class ComplexFunctionConsumer(WXFConsumer):
    """Implement a consumer that maps Complex to python complex types."""
    
    Complex = wl.Complex

    def build_function(self, head, args, **kwargs):
        # return a built in complex if head is Complex and argument length is 2.
        if head == self.Complex and len(args) == 2:
            return complex(*args)
        # otherwise use the super method (default case).
        else:
            return super().build_function(head, args, **kwargs)

# TODO: Change this value with a valid local path to a Wolfram Kernel.
kernel_path = '/Applications/Wolfram Desktop.app/Contents/MacOS/WolframKernel'
with WolframLanguageSession(kernel_path) as session:
    # define a matrix of complex numbers:
    # (  1+i  1 )
    # ( -1+i  i )
    complex_array = [[complex(1, 1), complex(-1, 1)], [1, complex(0, 1)]]
    # standard evaluation uses the default WXF parser:
    res = session.evaluate(wl.Eigenvalues(complex_array))
    print(res.get())  # [Complex[ << 2 >> ], 0.0]
    # alternativelly, get the WXF bytes and parse them using the complex consumer:
    complex_result = binary_deserialize(res.wxf, consumer=ComplexFunctionConsumer())
    print(complex_result)  # [(1+2j), 0.0]
