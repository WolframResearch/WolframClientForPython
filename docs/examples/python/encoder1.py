from __future__ import absolute_import, print_function, unicode_literals

from wolframclient.language import wl
from wolframclient.serializers import export, wolfram_encoder

# define a new class.
class Animal(object):
    pass

# register a new encoder for instances of the Animal class.
@wolfram_encoder.dispatch(Animal)
def encode_animal(serializer, animal):
    # encode the class as a symbol called Animal
    return serializer.encode(wl.Animal)

# create a new instance
animal = Animal()
# serialize it
result = export(animal)
print(result) # b'Animal'