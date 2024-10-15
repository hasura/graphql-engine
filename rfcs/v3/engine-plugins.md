# Engine-plugins in Hasura V3

This document focuses on the implementation details for HTTP-based engine
plugins.

## Pre-parse Hook

For a pre-parse plugin, the request to the plugin is performed just after
receiving the request to the engine.

### Configuration

The pre-parse plugin can be configured using an OpenDD object of kind `LifecyclePluginHook`. It includes the following information:

1. The engine-plugin URL
2. Request Includes (this can be used to optimize critical engine plugins):
    1. Request Headers
    2. Graphql request
    3. Variables

Please note that the presence of `operationName` is not configurable, and
including/excluding operation name won't have much impact on the request size.

An example of configuration JSON is:

```json
{
  "kind": "LifecyclePluginHook",
  "version": "v1",
  "definition": {
    "pre": "parse",
    "name": "test",
    "url": "http://localhost:8787",
    "config": {
      "request": {
        "headers": {
          "additional": {
            "hasura-m-auth": {
              "value": "zZkhKqFjqXR4g5MZCsJUZCnhCcoPyZ"
            }
          }
        },
        "session": {},
        "rawRequest": {
          "query": {},
          "variables": {}
        }
      }
    }
  }
}
```

### Request

The request to the pre-parse hook should have sufficient information to cater to
the following planned use cases:

1. Rate limits
2. Depth limits
3. Node limits
4. Caching (get-cache)

The request should have the following:

1. Headers: Include information for the uniqueness of the request (origin,
   session variables, etc.), cache control information, etc.
2. Hasura’s session information: Role and session variables
3. Raw request: Raw request received by graphql-engine (including variables)

```json
{
  "session": <the hasura session object>,
  "rawRequest": <raw request>
}
```

### Response

The response of a pre-parse hook can be of three types:

1. Return with a response: The engine-plugin has handled the request, and the
   graphql-engine should return the response provided by the engine-plugin.
   (Should we check if the response is valid according to the spec?)
2. Continue with the execution: The graphql-engine should proceed with the
   request handling.
3. Error response: Abort the request with the error response.

As suggested by @SamirTalwar, we can also use HTTP status codes to decide the
type of the response, i.e.

1. 200s HTTP status code will mean either:
   1. 200: A successful response
   2. 204: Or continued execution
2. 400 HTTP status code will mean user error
3. 500 HTTP status code will mean an internal error

#### Success response

HTTP code: 200

```
<the response json value>
```

#### Continue with execution

HTTP code: 204 There should be no response body for this case

#### Error

A pre-plugin response can be of two types:

1. User error: This will include errors that can be propagated to the user.

     HTTP code: 400

     ```
     <The user error json value>
     ```

2. Internal error: Internal errors are encountered while handling the request.
   The engine-plugin can dictate the engine to either abort the execution or
   continue with the request. The internal errors will not be propagated to the
   users; they will only be part of the traces.

     HTTP code: 500
     ```json
     {
        "details": <The internal error json value>,
        "action": <abort/continue>
     }
     ```

## Pre-response hook

A pre-response hook is called just before returning a response to the user. For
now, we will have asynchronous pre-response hooks only.

An asynchronous hook will be useful for the following use cases:

1. Caching (cache-set)
2. Custom business logic: Send mail/slack notifications for mutations

### Configuration

Like a pre-parse hook, a pre-response hook can also be configured using a
configuration file. The request can be configured to omit a few fields if
needed.

An example of configuration JSON is:

```json
{
  "kind": "LifecyclePluginHook",
  "version": "v1",
  "definition": {
    "pre": "response",
    "name": "test",
    "url": "http://localhost:8787",
    "config": {
      "request": {
        "headers": {
          "additional": {
            "hasura-m-auth": {
              "value": "zZkhKqFjqXR4g5MZCsJUZCnhCcoPyZ"
            }
          }
        },
        "session": {},
        "rawRequest": {
          "query": {},
          "variables": {}
        },
        "response": {}
      }
    }
  }
}
```

### Request

A pre-response hook’s request can have the following fields:

1. Raw request: The raw request for which the engine has generated the response.
2. Session: The role and session variables
3. Engine’s response: The response that we have generated after executing the
   query.
4. Request headers: This can be important for caching engine plugins

```json
{
  "session": <the hasura session object>,
  "rawRequest": <raw request>,
  "response": <engine's response>
}
```

### Response

For asynchronous pre-response hook, the request can be either of the two:

1. Success
2. Error

#### Async Success Response

HTTP Code: 200s

```
There need not be any response body.
```

#### Async Error Response

HTTP code 400s

```
<optional error details as JSON>
```

The error details will be part of the traces.

## Multiple engine-plugins

The engine can handle multiple engine plugins.

### Pre-plugins

For example, multiple pre-plugins can be thought of as a pipeline:

```
              _____________________       ______________________       __________________
             |                     |     |                      |     |                  |
  Request--->|  Pre-parse Plugin 1 |---->|  Pre-parse Plugin 2  |---->| Engine Execution |--...
             |_____________________|     |______________________|     |__________________|
```

For plugin 2, we will do the following:

- If plugin 1 responds successfully/error, we will NOT call plugin 2, and there
  will be a short-circuit.
- Only for the continued execution case will we call plugin 2.
- The request to all the pre-plugin will be the same (the raw request and
  session information are not going to change)

### Pre-response

Multiple pre-response engine plugins can also be handled. Since they are async
in nature, we can execute them in parallel:

```
Engine execution ------> To the user
                  |      ________________
                  |     |      Async     |
                  |---->|  Pre-response  |
                  |     |     Plugin 1   |
                  |     |________________|
                  |      ________________
                  |     |      Async     |
                  |---->|  Pre-response  |
                  |     |     Plugin 2   |
                  |     |________________|
                  ...
```

## How will this look in the metadata?

Engine plugins will be part of the metadata (OpenDD). This will be more like the
`AuthConfig` and will be handled while building the artifacts.

The engine-plugin artifacts will be similar to how we store `AuthConfig`
artifacts right now. We will have new artifacts (pre-parse and pre-response
plugin artifacts).

Each artifact will have a list of engine plugins in the order of execution. For
example:

```
 __________________
|  ______________  |
| | Pre-parse 1  | |                      ________________       ________________       __________________
| |______________| |                     |                |     |                |     |                  |
|  ______________  | =====>   Request--->|  Pre-Parse 1   |---->|  Pre-Parse 2   |---->| Engine Execution |--...
| | Pre-parse 2  | |                     |________________|     |________________|     |__________________|
| |______________| |
|__________________|
```

For pre-response, the order doesn’t matter right now, but we will still maintain
an order (to future-proof for synchronous pre-response).

There are a few caveats with the ordering of engine plugins for the multitenant
engine or DDN cloud: Auth plugin (once converted to an engine plugin, will
always be executed first).

## Future plans

### Synchronous pre-response hook

A synchronous hook can be useful for response transformation using something
like kriti-lang.

For synchronous pre-response hooks, the response can be similar to the pre-parse
hook. I.e., it can be one of the three: Return with a response: The engine
plugin has handled the request, and the graphql-engine should return the
response provided by the engine plugin (and ignore the response generated by the
engine). Return with engine’s response: The graphql-engine should proceed with
the engine’s response. Error response: Abort the request with the error
response.

Synchronous pre-response engine-plugins will be daisy-chained with one another:

```
                      __________________       __________________
                     |                  |     |                  |
Engine execution --->|  pre-response 1  |---->|  pre-response 2  |----> ...
                     |__________________|     |__________________|
```

For synchronous pre-response, the response will be the response from the
previous node (i.e., for response 1, the response will be generated by the
engine, but for pre-response 2, it will be dependent on pre-response 1). Here
also, in case of an error response, we will short-circuit the execution stack.

#### Mixing synchronous and asynchronous pre-response

In case there are multiple synchronous as well as asynchronous pre-response, the
execution stack will look like this: First, we will handle all the synchronous
pre-response. In the end, we will handle the asynchronous ones.

```
                      _________________       _________________
                     |      Sync       |     |      Sync       |
Engine execution --->|  pre-response 1 |---->|  pre-response 2 |-------> To the user
                     |_________________|     |_________________|  |      _________________
                                                                  |     |      Async      |
                                                                  |---->|  pre-response 1 |
                                                                  |     |_________________|
                                                                  |      _________________
                                                                  |     |      Async      |
                                                                  |---->|  pre-response 2 |
                                                                  |     |_________________|
                                                                  ...
```
