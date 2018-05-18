# Cloud Evaluation

This sub-module provides tools to query api functions deployed on a Wolfram Cloud. The core component is the `WolframCloudSession` class, which once initialized, provide a simple way to make calls to APIs.

# Making a API call

Here is a minimalist example demoing how to call a public API sitting on the Wolfram public Cloud:
``` Python
from wolframclient.evaluation.cloud.cloudsession import WolframCloudSession

# start a session target the wolfram public cloud
session = WolframCloudSession()
# call the API
api_call = session.call('https://www.wolframcloud.com/objects/userXXX/public/foo/bar')
# fetch the output based on the success status:
if api_call.success:
	result = api_call.output
else:
	result = api_call.failure
```

## Cloud session

A cloud session, the python class `WolframCloudSession`, represents a persistent session to a target Wolfram Cloud. The session is eventually authenticated using OAuth. A cloud session can be used to call more than one API.
The default constructor with no argument provides a convenient way to initialize a cloud session targeting the Wolfram public cloud, but sometimes you may want to use a different server, and initialize your session with it:
```Python
from wolframclient.evaluation.cloud.server import Server
server = Server(...)
session = WolframCloudSession(server)
```

### Server

A server instance stores the API endpoint, which is similar to the variable `$CloudBase` of the Wolfram Language, and the authentication end points from which to fetch the request token and the access token. This is what a server initialization looks like in the case of the Wolfram public Cloud:

``` Python
from wolframclient.evaluation.cloud.server import Server

api_endpoint='https://www.wolframcloud.com'
requester_token_endpoint='https://account.wolfram.com/auth/request-token'
access_token_endpoint='https://account.wolfram.com/auth/access-token'

server = Server(
    api_endpoint, 
    requester_token_endpoint,
    access_token_endpoint)
```

Server parameters is rarely something one would store in the code, but rather in a file. 

### User credentials

Most of the time API have restricted access. In such cases it's necessary to authenticate and sign requests. Similarly to the `Server` class, the library provides two classes to represent an authenticated consumer:
- User credentials authenticates the user with its user ID and password (xauth)
- Secured authenticated key (aka. SAK) authenticates a consumer with a token and secret. SAK are generated in the Wolfram Language using `GenerateSecuredAuthenticationKey[]`.

Again this is the kind of information that should not be hard-coded, but rather load from a file.

### Authenticate

At this point we have seen how to build a session, eventually to a specific server, and how to store private information into classes. We now have all the pieces to build an authenticated session:
```Python
from wolframclient.evaluation.cloud.oauth import UserCredentials
from wolframclient.evaluation.cloud.cloudsession import WolframCloudSession

user_cred = UserCredentials('wolfram ID', 'password')
session = WolframCloudSession(authentication=user_cred)

print(session.authorized)
```
The session parameter `session.authorized` indicates the current status of the session with a reasonable precision.

## Calling API

In the previous section we built a cloud session, that we now are going to use to make signed request to APIs.

The most simple call targets an API without input parameters. The function `call` most of the time does not raise exception, it has a boolean `success` that instead indicates whether or not the call succeeded.

```Python
from wolframclient.evaluation.cloud.cloudsession import WolframCloudSession
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

Calling an API using named parameter is done through the named argument `input_parameters` which accepts a dictionary with string keys and arbitrary values. Suppose the following API was deployed to a Wolfram Cloud:
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

The result of an API call is returned as bytes.


### Encoding input parameters

It is possible to encode the input to some specific formats: 
- `wxf`, 
- `json` 
The input parameters are encoded using either `json.dumps` or `wolframclient.serializers.export`.

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
        'b__json' : json.dumps(B)
    })
```