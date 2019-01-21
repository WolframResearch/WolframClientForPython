from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl
from wolframclient.deserializers import WXFConsumer, binary_deserialize

import math
import fractions

# define Complex symbol once and for all
Complex = wl.Complex

class MathConsumer(WXFConsumer):
    """Implement a consumer with basic arithmetic operation."""
    
    # Specific convertion for Pi, other symbols use the default method.
    def consume_symbol(self, current_token, tokens, **kwargs):
        # Convert symbol Pi to its numeric value as defined in Python
        if current_token.data == 'Pi':
            return math.pi
        else:
            return super().consume_symbol(current_token, tokens, **kwargs)

    # Associate heads with the method to convert them to Python types.
    DISPATCH = {
        Complex: 'build_complex',
        wl.Rational: 'build_rational',
        wl.Plus: 'build_plus',
        wl.Times: 'build_times'
    }
    # Overload the method that builds functions. 
    def build_function(self, head, args, **kwargs):
        # check if there is a specific function associated to the function head
        builder_func = self.DISPATCH.get(head, None)
        if builder_func is not None:
            try:
                # get the class method and apply it to the arguments.
                return getattr(self, builder_func)(*args)
            except Exception:
                # instead of failing, fallback to default case.
                return super().build_function(head, args, **kwargs)
        # heads not listed in DISPATCH are delegated to parent's method
        else:
            return super().build_function(head, args, **kwargs)

    def build_plus(self, *args):
        total = 0
        for arg in args:
            total = total + arg
        return total

    def build_times(self, *args):
        total = 1
        for arg in args:
            total = total * arg
        return total

    def build_rational(self, *args):
        if len(args) != 2:
            raise ValueError('Rational format not supported.')
        return fractions.Fraction(args[0], args[1])
    
    def build_complex(self, *args):
        if len(args) != 2:
            raise ValueError('Complex format not supported.')
        return complex(args[0], args[1])

with WolframLanguageSession() as session:
    array = [
        [wl.Pi, -2, 0], 
        [1, wl.Pi, -1],
        [0, 2, wl.Pi]]

    # expression to evaluate: Eigenvalues[array]
    expr = wl.Eigenvalues(array)

    # Eigenvalues are exact, but the result is a symbolic expression:
    # [Times[Rational[1, 2], Plus[Complex[0, 4], Times[2, Pi]]], 
    # Times[Rational[1, 2], Plus[Complex[0, -4], Times[2, Pi]]], Pi]
    print(session.evaluate(expr))
    
    # Use evaluate_wxf to evaluate without deserializing the result.
    wxf = session.evaluate_wxf(expr)
    # deserialize  using the math consumer:
    complex_result = binary_deserialize(wxf, consumer=MathConsumer())
    # get a numerical result, only made of built-in Python types.
    # [(3.141592653589793+2j), (3.141592653589793-2j), 3.141592653589793]
    print(complex_result)
