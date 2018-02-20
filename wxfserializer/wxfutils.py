# Utility function.
def write_varint(int_value, data_consumer):
    """Serialize `int_value` into varint bytes and append them to 
    `data_consumer`.
    """
    while True:
        next = int_value & 0x7f
        int_value >>= 7
        if int_value:
            data_consumer.append(next | 0x80)
        else:
            data_consumer.append(next)
            break

INT8_MAX = 1 << 7
INT8_MIN = -(1 << 7)
INT16_MAX = 1 << 15
INT16_MIN = -(1 << 15)
INT32_MAX = 1 << 31
INT32_MIN = -(1 << 31)
INT64_MAX = 1 << 63
INT64_MIN = -(1 << 63)