from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl
# conveniently import Global as g
from wolframclient.language import Global as g

with WolframLanguageSession() as session:
    # The function max belongs to context Global`
    session.evaluate('max[s : List[__String]] := MaximalBy[s, StringLength]')
    # Global`max is g.max in Python
    print(g.max)
    # Evaluate the function on a Python list of strings
    res = session.evaluate(g.max(['hello', 'darkness', 'my', 'old', 'friend']))
    print(res)
