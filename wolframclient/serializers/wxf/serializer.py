# -*- coding: utf-8 -*-

from __future__ import absolute_import, print_function, unicode_literals

__all__ = [
    'WXFExprSerializer',
    'SerializationContext',
    'write_varint',
    'WXFSerializerException'
    ]

class WXFSerializerException(Exception):
    pass

def write_varint(int_value, data_consumer):
    """Serialize `int_value` into varint bytes and append them to
    `data_consumer`, return the number of bytes written.
    """
    if int_value < 0:
        raise TypeError('Negative values cannot be encoded as varint.')
    count = 0
    while True:
        count+=1
        next = int_value & 0x7f
        int_value >>= 7
        if int_value:
            data_consumer.append(next | 0x80)
        else:
            data_consumer.append(next)
            break

    return count

class SerializationContext(object):
    """ Keeps track of various parameter associated to an expression being serialized.
    The finalized expression has a tree structure; it is serialized depth first. During
    the serialization process of an expression involving many non-atomic elements (e.g List),
    we end up having incomplete parts at various level.

    For each level of the expression tree we have to remember the expected length, the number
    of elements already inserted, and whether or not the node is an association. The first two
    parameters prevent inconsistencies in the number of elements and the declared length, the
    last one avoid incorrect use of `WXFExprRule(Delayed)` tokens.
    """

    __slots__ = '_depth', '_expected_length_stack', '_current_index_stack', '_in_assoc_stack'

    def __init__(self):
        # first level has index 0 and lenght 1. It's the root of the expr
        # but does not represent anything in the final WL expr.
        self._depth = 0
        self._expected_length_stack = [1]
        # index starting at 0.
        self._current_index_stack = [0]
        # root is not an assoc.
        self._in_assoc_stack = [False]
    
    def _check_insert(self):
        if self._depth >= 0 and self._current_index_stack[self._depth] >= self._expected_length_stack[self._depth]:
            raise IndexError('Out of bound, number of parts is greater than declared length %d.' %
                             self._expected_length_stack[self._depth])

    def _step_out_finalized_expr(self):
        while self._depth >= 0 and self._current_index_stack[self._depth] == self._expected_length_stack[self._depth]:
            self._depth -= 1


    def add_part(self):
        self._check_insert()
        self._current_index_stack[self._depth] += 1
        # This is the best place to output the context. Otherwise index values can be confusing.
        # print(self)
        self._step_out_finalized_expr()

    @staticmethod
    def _set_at_index_or_append(array, index, value):
        '''Set the element of an `array` at a given index if it exists,
        append it to the array otherwise. The `index` must be at most the
        length of the array.
        '''
        if len(array) == index:
            array.append(value)
        elif len(array) > index:
            array[index] = value
        else:
            raise IndexError('Index {} is greater than array length: {}'.format(index, len(array)))

    def step_in_new_expr(self, length, is_assoc = False):
        # increment the index
        self.add_part()
        # go down one level in the expr tree, into the new expr.
        self._depth += 1
        # set or append element at index self._depth
        SerializationContext._set_at_index_or_append(self._expected_length_stack, self._depth, length)
        SerializationContext._set_at_index_or_append(self._current_index_stack, self._depth, 0)
        SerializationContext._set_at_index_or_append(self._in_assoc_stack, self._depth, is_assoc)

        if len(self._expected_length_stack) <= self._depth:
            self._expected_length_stack.append(length)
        else:
            self._expected_length_stack[self._depth] = length

        if len(self._current_index_stack) <= self._depth:
            self._current_index_stack.append(0)
        else:
            self._current_index_stack[self._depth] = 0
        
        self._step_out_finalized_expr()

    def is_valid_final_state(self):
        return self._depth == -1

    def is_in_assoc(self):
        return self._in_assoc_stack[self._depth]

    def __str__(self):
        return 'SerializationContext(depth={}, element={}/{})'.format(self._depth, self._current_index_stack[self._depth], self._expected_length_stack[self._depth])

WXF_VERSION = ord('8')
WXF_HEADER_SEPARATOR = ord(':')

class WXFExprSerializer(object):
    """ Pulls instances of `WXFExpr` from an `WXFExprProvider`, serializes them into wxf bytes and appends the data to
    a `WXFDataConsumer`. Ensures the output data is a valid WXF encoded expression, and raises an exception otherwise.

    See `tutorial/WXFFormatDescription` from Mathematica documentation or visit http://reference.wolfram.com/language/tutorial/WXFFormatDescription.html
    for an in depth description of the format.
    """
    __slots__ = '_expr_provider', '_data_consumer', '_context'

    def __init__(self, expr_provider, data_consumer):
        self._expr_provider = expr_provider
        self._data_consumer = data_consumer
        self._context = SerializationContext()

    @property
    def context(self):
        return self._context

    def serialize(self, pyExpr):
        '''
        Serialize the python expression given as parameter.
        '''
        self._data_consumer.append(WXF_VERSION).append(WXF_HEADER_SEPARATOR)
        for wxfexpr in self._expr_provider.provide_wxfexpr(pyExpr):
            wxfexpr._serialize_to_wxf(self._data_consumer, self._context)
        if not self._context.is_valid_final_state():
            raise WXFSerializerException('Inconsistent state: truncated expr.')
