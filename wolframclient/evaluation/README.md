# Cloud Evaluation

This sub-module provides tools to query api functions deployed on a Wolfram Cloud. The core component is the `CloudSession` class, which once initialized, provide a simple way to make calls to APIs.

# Making a API call

Here is a minimalist example demoing how to call a public API sitting on the Wolfram public Cloud:
``` Python
from wolframclient.evaluation.cloud.cloudsession import CloudSession

# start a session target the wolfram public cloud
session = CloudSession.default()
# call the API
api_call = session.call('https://www.wolframcloud.com/objects/userXXX/public/foo/bar')
# fetch the output based on the success status:
if api_call.success:
	result = api_call.output
else:
	result = api_call.failure
```

## Cloud session

A cloud session, the python class `CloudSession`, represents a persistent session to a target Wolfram Cloud. The session is eventually authenticated using OAuth. A cloud session can be used to call more than one API.
The `default` method provide a convenient way to initialize a cloud session targeting the Wolfram public cloud, but sometimes you may want to use a different server, and initialize your session with it:
```Python
from wolframclient.evaluation.cloud.server import Server
server = Server(...)
session = CloudSession(server)
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

Server parameters is rarely something one would store in the code, but rather in a file. Static helper methods allow automatic configuration from a file:

```Python
server = Server.from_file('/path/to/server_config.cfg')
```

When finer control is required, the `Configuration` class is designed to provide more flexibility at the cost of few more lines of code. It helps represent the content of a [configuration file](https://docs.python.org/2/library/configparser.html) as a structured data structure.

### User credentials

Most of the time API have restricted access. In such cases it's necessary to authenticate and sign requests. Similarly to the `Server` class, the library provides two classes to represent an authenticated consumer:
- User credentials authenticates the user with its user ID and password (xauth)
- Secured authenticated key (aka. SAK) authenticates a consumer with a token and secret. SAK are generated in the Wolfram Language using `GenerateSecuredAuthenticationKey[]`.

Again this is the kind of information that should not be hard-coded, but rather load from a file. 
```Python
from wolframclient.evaluation.cloud.oauth import UserCredentials

user_cred = UserCredentials.from_file('/private/etc/my_cloud_credentials.cfg')
```
```Python
from wolframclient.evaluation.cloud.oauth import SecuredAuthenticationKey

secured_key = SecuredAuthenticationKey.from_file('/private/etc/my_app_sak.cfg')
```

### Authenticate

At this point we have seen how to build a session, eventually to a specific server, and how to store private information into classes. We now have all the pieces to build an authenticated session:
```Python
from wolframclient.evaluation.cloud.oauth import UserCredentials
from wolframclient.evaluation.cloud.cloudsession import CloudSession

user_cred = UserCredentials.from_file('/private/etc/my_cloud_credentials.cfg')
session = Session.default()

session.authenticate(user_cred)
```
The session parameter `session.authorized` indicates the current status of the session with a reasonable precision.

## Calling API

In the previous section we built a cloud session, that we now are going to use to make signed request to APIs.

### Basic API call

The most simple call targets a API without input parameters.

### Configuration

Setting up a configuration is done in two parts. First the initialization where the scheme is provided. The scheme is a list of sections to look for, and a list of parameters to expect in each of these sections. The library provide three schemes representing user credentials configuration and server configuration. Classes that rely on a configuration to be initialized provide static initialization methods that deal with configuration automatically, most users should not have to manipulate configuration instances at all.

Below is a simple configuration expecting at least one section `[User]` with at least two parameters `id` and `password`:
``` Python
config = Configuration(
        {'User': ['id', 'password']}
    )
```

The configuration can now be populated with data using one of its `read` methods. Say we have the following configuration file `/private/etc/user.cfg`:

```
[User]
id = my_wolfram_id
password = Password1!

[An other section]
paramA = valueA

```
We populate the configuration using the file:
``` Python
config.read('/private/etc/user.cfg')
```
The instance `config` now has two new attributes `user_id` and `user_password`, built from the section name and the parameter name in lower case. Note that sections present in the file but not declared during initialization are simply ignored.

It is recommended to use the following built-in configuration initializers when possible:
```Python
import wolframclient.evaluation.configuration as config

# server endpoints
empty_server_config = config.server_configuration()
# user id and password
empty_user_config = config.user_credential_configuration()
# secured authentication key
empty_sak_config = config.sak_configuration()
```