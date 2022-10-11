Overview
========

GraphQL allows for rapid iteration on API design and usage during development. On the other hand, REST APIs are inflexible but have the benefit of twenty years of usage in production, optimizations and tooling. We can get the best of both worlds: given a fixed GraphQL query or mutation, Hasura can create an idiomatic REST endpoint, allowing us to make use of existing REST API tooling, and to benefit from optimizations such as caching in the browser or at a CDN.

This document will specify the REST endpoints that Hasura will generate from these GraphQL operations.

Specification
=============

Overview
--------

Hasura’s metadata includes several defined *endpoints*, each of which consists of the following data:

-   A name, used as the primary key in Hasura’s metadata (but otherwise unused)

-   A URL template

-   A set of acceptable HTTP methods

-   A query or mutation, specified without variables

For example:

-   **Name:** `user\_by\_id`

-   **URL template:** `/users/:user\_id`

-   **Methods**: `GET`, `POST`

-   **Query**:
    
    ```
    query @cached ($user\_id: String!) {
      users(where: { id: { \_eq: $user\_id } }) {
        name
        email
        role
      }
    }
    ```

In the following sections, we’ll explain the semantics of each of these parts.

URL Templates
-------------

A URL template consists of a sequence of *path parts*, each of which is either a *path literal* or a *path parameter:*

```
Part := “:“, segment-nz-nc ; path parameter
      | segment-nz-nc      ; path literal

Template := \*(“/”, Part)  ; URL template
```

where segment-nz-nc (non-zero-length segment without colon) is defined in RFC 3986 §1.1.1.

The example /users/:user\_id consists of two parts: a path literal “users”, followed by a path parameter named “user\_id”. Note that the name of the path parameter matches the name of the only specified variable in the GraphQL query.

In general, every path parameter must correspond to a defined query variable, but the converse need not hold, since we also permit variables to be specified via the query part of the URL, or via the request body (see **Variables**).

HTTP Methods
------------

-   The list of valid HTTP methods should be used to determine the correct endpoint for a given HTTP request (see **Routing**)

-   For query operations, only GET and POST should be permitted. Any other method should result in a validation error.

-   For mutation operations, GET should be disallowed, resulting in a validation error.

Routing
-------

Given a set of defined endpoints, HTTP request URL and HTTP request method, the determination of the correct endpoint happens according to the following algorithm:

1.  Split the path component of the HTTP request URL into segments, as described in RFC 3986.

2.  For each defined endpoint:

    1.  Check that the HTTP request method matches one of the defined endpoint’s accepted methods. If not, continue to the next defined endpoint.

    2.  Check that the number of path parts in the URL template matches the number of segments in the HTTP request URL. If not, continue to the next defined endpoint.

    3.  For each segment/part pair in order:

        1.  If the path part is a literal, check that the segment text matches the literal. If not, continue to the next defined endpoint.

        2.  If the path part is a parameter, store a mapping from the parameter name to the segment text.

3.  At this point, for each endpoint, we may or may not have determined a match, and in the case of a match, a set of assignments from parameter names to segments.

4.  If there are multiple matching endpoints, return a 500 Internal Server Error response. Overlapping endpoints should have been detected during validation (see **Overlapping Endpoints**).

5.  If there is exactly one matching endpoint: return that endpoint and the mapping from parameter names to variable values:

    1.  For each parameter name/segment pair:

        1.  Determine the type of the corresponding GraphQL query variable from the query definition, and parse the segment text as a value of that type accordingly (see **Parameter Types**)

6.  If there is no defined endpoint matching the URL template and HTTP methods, then an error code should be returned:

    1.  If there is no defined endpoint for the same URL template and any other HTTP method, then a 404 Not Found error should be returned

    2.  If there is a defined endpoint for the same URL template, but for a different HTTP method, then a 405 Method Not Allowed error should be returned, along with the list of supported methods in the Allow: header.

In the running example, the only defined endpoint matches the request GET /users/abc123, but does not match any of the following example requests:

-   `GET /users`

-   `GET /users/abc123/purchases`

-   `PUT /users/abc123`

Variables
---------

GraphQL variables can be provided in one of three ways:

1.  Via a path parameter segment of the URL template (scalar-type variables only)

2.  Via the query portion of the URL (scalar-type variables only)

3.  In the request body:

    1.  encoded as a JSON object, with
        `Content-Type: application/json`, or

    2.  encoded as key/value pairs, with
        `Content-Type: application/x-www-form-urlencoded`

Duplicate variables with the same name will result in a 400 Bad Request error.

No matter how the variables are provided, they will be combined into a single collection of key/value pairs, and passed to the underlying GraphQL operation, as if they had been provided in the variables section of an explicitly-executed GraphQL request. Any additional checking of variables from that point will proceed as if the user had made such a request.

In the running example, the `user\_by\_id` query captures the user ID in the URL template, but an additional endpoint could instead choose to capture it as a query argument or in the request body. For example, consider the following endpoint which is a slight modification of the first example:

-   **Name:** `get\_user`

-   **URL template:** ``/users/get`

-   **Methods**: `GET, POST`

-   **Query:** `query @cached ($user\_id: String!) { … }`

The client can call this new endpoint in several ways:

-   ```bash
    curl -X GET /users/get?user\_id=abc123
    ```

-   ```bash
    curl -X POST /users/get?user\_id=abc123
    ```

-   ```bash
    curl -X POST /users/get
    -d '{ "user\_id": "abc123" }'
    -H 'Content-Type: application/json'
    ```

-   ```bash
    curl -X POST /users/get
    -d 'user\_id=abc123'
    -H 'Content-Type: application/x-www-form-urlencoded'
    ```

Parameter Types
---------------

Variables which are provided via the URL (in either the path or query portions) are limited to primitive (scalar) types only. That is, they must correspond to a variable with one of the primitive GraphQL types: String, ID, Int, Float or Boolean.

Any value with one of these variable types will be parsed from the decoded URL text as if encoded as JSON literals, per the definitions in RFC 7159. Values of type String and ID will be decoded as JSON string literals, values of type Boolean as JSON boolean literals, and values of type Int and Float as JSON numeric literals.

Parsing for nullable types and list types is not supported, but may be added in a future version of this document.

Response Headers
----------------

The server will support caching on the client by providing the Cache-Control header. In order to enable this behavior, the query should include the @cached directive (see the running example above). The returned Cache-Control header will include a max-age argument which indicates the remaining lifetime of any server-side cache of the returned data.

Response Codes
--------------

Generated endpoints will return standard HTTP error codes for user errors and configuration errors. Specifically, but not limited to:

-   **400 Bad Request** if the request is poorly formed (e.g. unexpected or incorrect query variable; in general, see the error message)

-   **404 Not Found** if an endpoint does not exist

-   **405 Method Not Acceptable** if an endpoint exists, but for a different HTTP method

-   **409 Conflict** if a request contains inconsistent data (e.g. overlapping endpoint definitions)

-   **500 Internal Server Error** if the internal state of the server was determined to be incorrect

Response Body
-------------

If an operation is successful, the response body will contain the contents of the data key of the equivalent GraphQL response.

If an operation is unsuccessful, the response body will contain a JSON representation of the error which occurred, in addition to a semantically-meaningful HTTP status code.

Validation
----------

### Overlapping Endpoints

Two endpoint definitions are *overlapping* if there is a valid HTTP request which would match both endpoints according to the definitions in the **Routing** section above.

During metadata validation, any metadata containing overlapping endpoints should be rejected, returning a **409 Conflict** error code along with the names of any overlapping endpoints.

### Operation Type

Validation should be performed based on the type of the underlying GraphQL operation:

-   If the type is **query**, the HTTP method should be GET or POST (POST is permitted in order to support non-primitive variable types)

-   If the type is **mutation**, the HTTP method must **not** be GET

-   Type **subscription** is disallowed (although this may change in the future)

### Variable Names and Types

-   Every variable mentioned in the URL template must appear as the name of a GraphQL variable of primitive type in the underlying operation.

Future Work
===========

Subscription Support
--------------------

It may be possible to support subscriptions in future, by relying on a standard such as *server-sent events* which would allow us to encode a one-way event stream on top of HTTP, with graceful degradation in terms of browser support.

Nested Error Support
--------------------

Currently, any top-level error will be converted into a HTTP error code, but we have not specified what the behavior should be for nested errors. It might also be possible to selectively turn nested errors into HTTP error codes by using GraphQL directives on the underlying operation.

Support for Nullable and List Types
-----------------------------------

The primitive type restriction on variables naturally extends to list and nullable types.

Generate Swagger Documentation and Metadata
-------------------------------------------

Stored comments on entities and fields could be used to generate Swagger/OpenAPI documentation and metadata.
