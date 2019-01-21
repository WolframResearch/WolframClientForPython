from wolframclient.language import wl
from wolframclient.serializers import export, wolfram_encoder

# define a hierarchy of classes.
class Animal(object):
    pass

class Fish(Animal):
    pass

class Tuna(Fish):
    pass

# will not have its own encoder.
class Salmon(Fish):
    pass

# register a new encoder for Animal.
@wolfram_encoder.dispatch(Animal)
def encode_animal(serializer, animal):
    return serializer.encode(wl.Animal)

# register a new encoder for Fish.
@wolfram_encoder.dispatch(Fish)
def encode_fish(serializer, animal):
    return serializer.encode(wl.Fish)

# register a new encoder for Tuna.
@wolfram_encoder.dispatch(Tuna)
def encode_tuna(serializer, animal):
    # encode the class as a function using class name
    return serializer.encode(wl.Tuna)


expr = {'fish' : Fish(), 'tuna': Tuna(), 'salmon': Salmon()}
result = export(expr)
print(result) # b'<|"fish" -> Fish, "tuna" -> Tuna, "salmon" -> Fish|>'