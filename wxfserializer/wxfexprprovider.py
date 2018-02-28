from wxfserializer.wxfencoder import DefaultWXFEncoder


class WXFExprProvider(object):
    '''
    Expression provider pull instances of WXFExpr from a given WXFEncoder `encoder`
    (nb: encoders can be chained). If none is provided the default class `DefaultWXFEncoder` 
    is used to instanciate one.
    
    The optional `default` function is used in last resort if no encoder handled the object.
    It is applied to the object and is expected to transform it to something serializable
    e.g: a string using `default=repr`.
    
    When a given object `o` was not encoded, the output of `default(o)` is passed to the 
    encoder.
    
    One must carefully design the `default` function to avoid stack overflow.
    '''
    __slots__ = 'encoder', 'default'

    def __init__(self, encoder=None, default=None):
        if encoder is not None:
            self.encoder = encoder
        else:
            self.encoder = DefaultWXFEncoder()
        if default is not None:
            self.encoder.default = default

    def pythonToWXFExpr(self, pythonExpr):
       for wxf_expr in self.encoder.encode(pythonExpr):
           yield wxf_expr
