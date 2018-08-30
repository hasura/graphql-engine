JWT Authorization
=================

You can configure JWT authorization mode (see :doc:`GraphQL server options
<../deployment/graphql-engine-flags/reference>`) to authorize all incoming
requests to Hasura GraphQL engine server.

The idea is - Your auth server will return JWT tokens, which is decoded and
verified by GraphQL engine to authorize and get metadata about the request
(``x-hasura-*`` values).


.. image:: ../../../img/graphql/manual/auth/jwt-auth.png

The JWT is decoded, the signature is verified and then it is asserted that the
current role of the user is in the list of allowed roles. If the authorization
passes, then all of the ``x-hasura-*`` values in the claim is used for the
permissions system.

.. note::
   Configuring JWT requires Hasura to run with an access key (``--access-key``).

   - The authorization is **enforced** when ``X-Hasura-Access-Key`` header is
     **not found** in the request.
   - The authorization is **skipped** when ``X-Hasura-Access-Key`` header **is
     found** in the request.

..   :doc:`Read more<config>`.


TL;DR
-----
1. The JWT must contain: ``x-hasura-default-role``, ``x-hasura-allowed-roles``
   in the claims.
2. Other optional ``x-hasura-*`` fields (required as per your defined
   permissions)
3. You can send ``x-hasura-role`` as header in the request to indicate a
   different role.
4. Send the JWT via ``Authorization: Bearer <JWT>`` header.
 

The Spec
--------
When your auth server generates the JWT, the claims in the JWT **must contain**
the following:

1. A ``x-hasura-default-role`` field : indicating the default role of that user
2. A ``x-hasura-allowed-roles`` field : a list of allowed roles for the user


The claims in the JWT, can have other ``x-hasura-*`` fields where their values
can only be strings. You can use these ``x-hasura-*`` fields in your
permissions.

Now, the JWT should be sent by the client to Hasura GraphQL engine via the
``Authorization: Bearer <JWT>`` header.

Example JWT claim:

.. code-block:: json

  {
    "sub": "1234567890",
    "name": "John Doe",
    "admin": true,
    "iat": 1516239022,
    "x-hasura-allowed-roles": ["editor","user", "mod"],
    "x-hasura-default-role": "user",
    "x-hasura-user-id": "1234567890",
    "x-hasura-org-id": "123",
    "x-hasura-custom": "custom-value"
  }

This contains standard (``sub``, ``iat`` etc.) and custom (``name``, ``admin``
etc.) JWT claims, as well as Hasura specific claims. Note that,
``x-hasura-default-role`` and ``x-hasura-allowed-roles`` are mandatory, rest of
them are optional.

.. note::

   All ``x-hasura-*`` values should be ``String``, they will be converted to the
   right type automatically.

The default role can be overriden by ``x-hasura-role`` header, while making a
request.

.. code-block:: http

   POST /v1alpha1/graphql HTTP/1.1
   Authorization: Bearer eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWI...
   X-Hasura-Role: editor

   ...


Configuring JWT mode
--------------------

You can enable JWT mode by using the ``--jwt-secret`` flag or
``HASURA_GRAPHQL_JWT_SECRET`` environment variable; the value of which is a
JSON.

The JSON is:

.. code-block:: json

   {
     "type": "<standard-JWT-algorithms>",
     "key": "<the-key>"
   }

``type``: Valid values are : ``HS256``, ``HS384``, ``HS512``, ``RS256``,
``RS384``, ``RS512``. (see https://jwt.io).

``HS*`` is for HMAC-SHA based algorithms. ``RS*`` is for RSA based signing. For
example, if your auth server is using HMAC-SHA256 for signing the JWTs, then
use ``HS256``. If it is using RSA with 512-bit keys, then use ``RS512``. EC
public keys are not yet supported.

``key``:

- Incase of symmetric key, the key as it is. (HMAC based keys).
- Incase of asymmetric keys, only the public key, in a PEM encoded string or as
  a X509 certificate.

Examples
^^^^^^^^

HMAC-SHA based
+++++++++++++++
Your auth server is using HMAC-SHA alogrithms to sign JWTs, and is using a
256-bit key. Then the JWT config will look like:

.. code-block:: json

   {
     "type":"HS256",
     "key": "3EK6FD+o0+c7tzBNVfjpMkNDi2yARAAKzQlk8O2IKoxQu4nF7EdAh8s3TwpHwrdWT6R"
   }
 
The ``key`` is the actual shared secret. Which is used by your auth server as well.

RSA based
+++++++++
Let's say your auth server is using RSA to sign JWTs, and is using a 512-bit
key. Then, the JWT config needs to have the only the public key, in PEM format
(not OpenSSH format):

.. code-block:: json

    {
      "type":"RS512",
      "key": "-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDdlatRjRjogo3WojgGHFHYLugd\nUWAY9iR3fy4arWNA1KoS8kVw33cJibXr8bvwUAUparCwlvdbH6dvEOfou0/gCFQs\nHUfQrSDv+MuSUMAe8jzKE4qW+jK+xQU9a03GUnKHkkle+Q0pX/g6jXZ7r1/xAK5D\no2kQ+X5xK9cipRgEKwIDAQAB\n-----END PUBLIC KEY-----\n"
    }

Running with JWT
^^^^^^^^^^^^^^^^
Using the flag:

.. code-block:: shell

  $ docker run -p 8080:8080 \
      hasura/graphql-engine:latest \
      graphql-engine \
      --database-url postgres://username:password@hostname:port/dbname \
      serve \
      --access-key mysecretkey \
      --jwt-secret '{"type":"HS256", "key": "3EK6FD+o0+c7tzBNVfjpMkNDi2yARAAKzQlk8O2IKoxQu4nF7EdAh8s3TwpHwrdWT6R"}'

Using env vars:

.. code-block:: shell

  $ docker run -p 8080:8080 \
      -e HASURA_GRAPHQL_ACCESS_KEY="mysecretkey" \
      -e HASURA_GRAPHQL_JWT_SECRET='{"type":"RS512", "key": "-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDdlatRjRjogo3WojgGHFHYLugd\nUWAY9iR3fy4arWNA1KoS8kVw33cJibXr8bvwUAUparCwlvdbH6dvEOfou0/gCFQs\nHUfQrSDv+MuSUMAe8jzKE4qW+jK+xQU9a03GUnKHkkle+Q0pX/g6jXZ7r1/xAK5D\no2kQ+X5xK9cipRgEKwIDAQAB\n-----END PUBLIC KEY-----\n"}' \
      hasura/graphql-engine:latest \
      graphql-engine \
      --database-url postgres://username:password@hostname:port/dbname \
      serve
