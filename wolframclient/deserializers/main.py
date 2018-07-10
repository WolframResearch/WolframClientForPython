import sys
sys.path.insert(0, '/Users/dorianb/Work/Mathematica/Workspaces/WolframClientForPython')
import logging
from wolframclient.deserializers import WXFToken, WXFParser, binary_deserialize, WXFConsumerNumpy, WXFConsumer
from wolframclient.serializers import export

def ntimes(value, N=100):
    for i in range(N):
        for val in value:
            yield val

# with open('/tmp/all.wxf', 'rb') as fp:
#     parser = WXFParser(fp)
#     for o in parser.tokens():
#         print(o)


value = u"maître & élève"
wxf = export(value, target_format='wxf')
o = binary_deserialize(wxf)
print(o == value)

# with open('/tmp/all.wxf', 'rb') as fp:
#     output = binary_deserialize(fp)
#     print(output)

# with open('/tmp/basic.wxf', 'rb') as fp:
#     try:
#         consumer = WXFConsumer()
#         print(binary_deserialize(fp, consumer=consumer))
#     except Exception as e:
#         print(e)
    
# with open('/tmp/basic.wxf', 'rb') as fp:
#     print(binary_deserialize(fp))

    # for o in parser.tokens():
    #     print(o)
    # print(parser.binary_deserialize())

# with open('/tmp/string_zip.wxf', 'rb') as fp:
#     parser = WXFParser(fp)
#     for o in parser.tokens():
#         print(o)

# with open('/tmp/all_zip.wxf', 'rb') as fp:
#     parser = WXFParser(fp)
#     for o in parser.tokens():
#         print(o)
