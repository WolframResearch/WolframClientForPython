# Wolfram Language cloud evaluation

The Client Library provides convenient methods to evaluate Wolfram Language expressions directly from Python. Ways to evaluate code include: direct evaluation by a Wolfram Cloud, either public or private, calling deployed API, local evaluation on a kernel.

## Expression evaluation

A one-shot evaluation on the Wolfram public cloud only requires to authenticate, e.g. using a WolframID and password:

```Python
from wolframclient import UserIDPassword, WolframCloudSession
userID = UserIDPassword('Wolfram ID', 'password')
session = WolframCloudSession(authentication=userID)
session.evaluate_string('Range[3]')
```

## Cloud functions

From an authenticated cloud session it is possible to build a cloud function, to later uses it with various parameters:

```Python
wl_str_reverse = session.cloud_function('StringReverse')
wl_str_reverse("hello from")
wl_str_reverse("the other side.")
```

Functions may accept more than one input parameters. The example below calls `Join` on multiple lists:
```Python
wl_join = session.cloud_function('Join[##] &')
wl_join([0,1], ["a", "b"], [2, "c"])
```

## API calls

From the Wolfram Language it is possible to deploy arbitrary code and expose it through an API.

Using the Wolfram Language, connect to the Wolfram Cloud:
```
CloudConnect['Wolfram ID', 'password']
```

Deploy an API accepting a list and returning the result of the function `MinMax` applied on it:
```
CloudDeploy[
    APIFunction[{"list" -> RepeatingElement[Expression]},
        MinMax[#list] &
    ],
    CloudObject["api/private/minmax"]
]
```
Note that the API was deployed without any particular permissions, and as such is a `Private` cloud object only usable by its owner.
Use the previously authenticated session to call the API from Python:
```Python
api = ('Wolfram ID', 'api/private/minmax')
result = session.call(api, {'list' : [[1, 2, 3], [-1, -2, -3]]})
if result.success:
    print('API call successfully returned:', result.output)
else:
    print('API failed:', result.failure)
```

# Cloud interactions documentation

## WolframCloudSession

A cloud session, the python class `wolframclient.WolframCloudSession`, represents a persistent session to a target Wolfram Cloud. The session is eventually authenticated using OAuth. A cloud session can be used to call more than one API, and it is recommanded to reuse a given session as much as possible to avoid unnecessary authentications. 
The default constructor with no argument provides a convenient way to initialize a cloud session targeting the Wolfram public cloud.

```Python
from wolframclient import WolframCloudSession
session = WolframCloudSession()
```

## Server

Sometimes you may want to use a different server, and initialize your session with it.
Server specifications requires to specify:
- the cloud base, the URL base used to build API URL. This value corresponds to the Wolfram Language symbol `$CloudBase`. E.g: `'https://www.wolframcloud.com'`
- the request token end point. The URL from which request tokens are fetch when authenticating using OAuth. E.g: `'https://account.wolfram.com/auth/request-token'`
- the access token end point. The URL from which request tokens are exchanged for an access token. E.g: `'https://account.wolfram.com/auth/access-token'`
- xauth consumer key and secret _(optional)_, if user and password authentication is used.
- a certificate _(optional)_, as a filename. E.g: `'/etc/ssl/certs/wolfram-ca.crt'`

```Python
from wolframclient import Server
server = Server(
    'https://www.wolframcloud.com',
    'https://account.wolfram.com/auth/request-token',
    'https://account.wolfram.com/auth/access-token')
# start an anonymous session
session = WolframCloudSession(server)
```

Server parameters is rarely something one would store in the code, but rather in a configuration file.

## Authentication classes

By default user deployed API have restricted access. In such cases it's necessary to authenticate and sign requests. Similarly to the `Server` class, the library provides two classes to represent an authenticated consumer:
- `wolframclient.UserIDPassword` authenticates the user with its user ID and password, using _xauth_.
- `wolframclient.SecuredAuthenticatedKey` (aka. SAK) authenticates a consumer with a token and secret. SAK are generated in the Wolfram Language using `GenerateSecuredAuthenticationKey`.

These parameters grants access to the user cloud credits and must be carefully stored.

Authenticate to the Cloud using a user account:
```Python
from wolframclient import UserIDPassword, WolframCloudSession
user_pwd = UserIDPassword('wolfram ID', 'password')
session = WolframCloudSession(authentication=user_pwd)
```
Authenticate using a secured key: 
```Python
from wolframclient import SecuredAuthenticatedKey
sak = SecuredAuthenticatedKey('aaabbbccc==', 'xxxyyyzzz==')
session = WolframCloudSession(authentication=sak)
```

The `WolframCloudSession` property `authorized` indicates the current status of the session with a reasonable precision.

## Calling API

We built a cloud session, that we now are going to use to make signed requests to APIs.

The most simple call targets an API without input parameters. The function `call` most of the time does not raise exception, but instead has a boolean `success` that indicates whether or not the call succeeded.

Call a public API using its URL:
```Python
from wolframclient import WolframCloudSession
session = WolframCloudSession()
response = session.call('https://www.wolframcloud.com/objects/dorianb/api/public/test_noparam')
# check the response status:
if response.success:
    print(response.output)
else:
    print(response.failure)
```
It's often more convenient to define the API using a tuple made of the user name and the API path. The precedent call is rewritten as:
```Python
api = ('dorianb', 'api/public/test_noparam')
session.call(api)
```

### Input parameters

Calling an API using named parameters is done through the named argument `input_parameters` which accepts a dictionary with string keys and arbitrary values. Suppose the following API was deployed to a Wolfram Cloud:
```
APIFunction[
    {"i" -> "Integer"}, 
    Range[#i] &
]
```
It is them possible to call it with:
```Python
response = session.call('cloud object url', input_parameters = {'i' : 5})
```

The result of an API call is returned as Python `bytes`.


### Encoding input parameters

It is possible to encode the input to some specific formats: 
- `wxf`, 
- `json`

The input parameters are encoded using either `json.dumps` or `wolframclient.export`.

```Python
session.call(url, 
    {'a': A, 'b' : B},
    target_format = 'json'
)
```
Is equivalent to:
```Python
session.call(url, 
    {
        'a__json': json.dumps(A), 
        'b__json': json.dumps(B)
    })
```