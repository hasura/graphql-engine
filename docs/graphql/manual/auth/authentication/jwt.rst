Authentication using JWT
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

You can configure GraphQL engine to use JWT authorization mode to authorize all incoming requests to Hasura GraphQL engine server.

The idea is - Your auth server will return JWT tokens, which is decoded and
verified by GraphQL engine to authorize and get metadata about the request
(``x-hasura-*`` values).


.. thumbnail:: ../../../../img/graphql/manual/auth/jwt-auth.png


The JWT is decoded, the signature is verified, then it is asserted that the
current role of the user (if specified in the request) is in the list of allowed roles.
If current role is not specified in the request, then the default role is picked.
If the authorization passes, then all of the ``x-hasura-*`` values in the claim
is used for the permissions system.

.. admonition:: Prerequisite
   
   It is mandatory to first :doc:`secure your GraphQL endpoint <../../deployment/securing-graphql-endpoint>` for the JWT mode to take effect.


In JWT mode, on a secured endpoint:

- JWT authentication is **enforced** when ``X-Hasura-Admin-Secret`` header is **not found** in the request.
- JWT authentication is **skipped** when ``X-Hasura-Admin-Secret`` header **is found** in the request and
  admin access is granted.


TL;DR
-----
1. The JWT must contain: ``x-hasura-default-role``, ``x-hasura-allowed-roles``
   in a custom namespace in the claims.
2. Other optional ``x-hasura-*`` fields (required as per your defined
   permissions).
3. You can send ``x-hasura-role`` as header in the request to indicate a role.
4. Send the JWT via ``Authorization: Bearer <JWT>`` header.


The Spec
--------
When your auth server generates the JWT, the custom claims in the JWT **must contain**
the following:

1. A ``x-hasura-default-role`` field : indicating the default role of that user i.e. the role that will be
   used in case ``x-hasura-role`` header is not passed
2. A ``x-hasura-allowed-roles`` field : a list of allowed roles for the user i.e. acceptable values of the
   ``x-hasura-role`` header

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
    "https://hasura.io/jwt/claims": {
      "x-hasura-allowed-roles": ["editor","user", "mod"],
      "x-hasura-default-role": "user",
      "x-hasura-user-id": "1234567890",
      "x-hasura-org-id": "123",
      "x-hasura-custom": "custom-value"
    }
  }

This contains standard (``sub``, ``iat`` etc.) and custom (``name``, ``admin``
etc.) JWT claims, as well as Hasura specific claims inside a custom namespace
(or key) i.e. ``https://hasura.io/jwt/claims``.

The ``https://hasura.io/jwt/claims`` is the custom namespace where all Hasura
specific claims have to be present. This value can be configured in the JWT
config while starting the server.

**Note**: ``x-hasura-default-role`` and ``x-hasura-allowed-roles`` are
mandatory, while rest of them are optional.

.. note::

   All ``x-hasura-*`` values should be ``String``, they will be converted to the
   right type automatically.

The default role can be overridden by ``x-hasura-role`` header, while making a
request.

.. code-block:: http

   POST /v1/graphql HTTP/1.1
   Authorization: Bearer eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWI...
   X-Hasura-Role: editor

   ...


Configuring JWT mode
--------------------

You can enable JWT mode by using the ``--jwt-secret`` flag or
``HASURA_GRAPHQL_JWT_SECRET`` environment variable; the value of which is a
JSON object:

.. code-block:: json

   {
     "type": "<standard-JWT-algorithms>",
     "key": "<optional-key-as-string>",
     "jwk_url": "<optional-url-to-refresh-jwks>",
     "claims_namespace": "<optional-key-name-in-claims>",
     "claims_format": "json|stringified_json",
     "audience": <optional-string-or-list-of-strings-to-verify-audience>,
     "issuer": "<optional-string-to-verify-issuer>"
   }

``key`` or ``jwk_url``, **one of them has to be present**.

``type``
^^^^^^^^
Valid values are : ``HS256``, ``HS384``, ``HS512``, ``RS256``,
``RS384``, ``RS512``. (see https://jwt.io).

``HS*`` is for HMAC-SHA based algorithms. ``RS*`` is for RSA based signing. For
example, if your auth server is using HMAC-SHA256 for signing the JWTs, then
use ``HS256``. If it is using RSA with 512-bit keys, then use ``RS512``. EC
public keys are not yet supported.

``key``
^^^^^^^
- In case of symmetric key (i.e. HMAC based key), the key as it is. (e.g. -
  "abcdef..."). The key must be long enough for the algorithm chosen,
  (e.g. for HS256 it must be at least 32 characters long).
- In case of asymmetric keys (RSA etc.), only the public key, in a PEM encoded
  string or as a X509 certificate.

This is an optional field. You can also provide a URL to fetch JWKs from using
the ``jwk_url`` field.

``jwk_url``
^^^^^^^^^^^
A URL where a provider publishes their JWKs (which are used for signing the
JWTs). The URL **must** publish the JWKs in the standard format as described in
https://tools.ietf.org/html/rfc7517

This is an optional field. You can also provide the key (certificate, PEM
encoded public key) as string as well - under the ``key`` field.

**Rotating JWKs**:

Some providers rotate their JWKs (E.g - Firebase). If the provider sends an
``Expires`` header with the response of JWK, then graphql-engine will refresh
the JWKs automatically. If the provider does not send ``Expires`` header, the
JWKs are not refreshed.

**Example**:

- Auth0 publishes their JWK url at: ``https://<YOUR_AUTH0_DOMAIN>.auth0.com``.
  But Auth0 has a bug. See known issues: :ref:`auth0-issues`.
- Firebase publishes their JWK url at:
  ``https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com``.

``claims_namespace``
^^^^^^^^^^^^^^^^^^^^
This is an optional field. You can specify the key name
inside which the Hasura specific claims will be present. E.g. - ``https://mydomain.com/claims``.

**Default value** is: ``https://hasura.io/jwt/claims``.


``claims_format``
^^^^^^^^^^^^^^^^^
This is an optional field, with only the following possible values:
- ``json``
- ``stringified_json``

Default is ``json``.

This is to indicate that if the hasura specific claims are a regular JSON object
or stringified JSON

This is required because providers like AWS Cognito only allows strings in the
JWT claims. `See #1176 <https://github.com/hasura/graphql-engine/issues/1176>`_.

Example:-

If ``claims_format`` is ``json`` then JWT claims should look like:

.. code-block:: json

  {
    "sub": "1234567890",
    "name": "John Doe",
    "admin": true,
    "iat": 1516239022,
    "https://hasura.io/jwt/claims": {
      "x-hasura-allowed-roles": ["editor","user", "mod"],
      "x-hasura-default-role": "user",
      "x-hasura-user-id": "1234567890",
      "x-hasura-org-id": "123",
      "x-hasura-custom": "custom-value"
    }
  }


If ``claims_format`` is ``stringified_json`` then JWT claims should look like:

.. code-block:: json

  {
    "sub": "1234567890",
    "name": "John Doe",
    "admin": true,
    "iat": 1516239022,
    "https://hasura.io/jwt/claims": "{\"x-hasura-allowed-roles\":[\"editor\",\"user\",\"mod\"],\"x-hasura-default-role\":\"user\",\"x-hasura-user-id\":\"1234567890\",\"x-hasura-org-id\":\"123\",\"x-hasura-custom\":\"custom-value\"}"
  }

``audience``
^^^^^^^^^^^^
This is an optional field. Certain providers might set a claim which indicates
the intended audience for the JWT. This can be checked by setting this field.

When this field is set, during the verification process of JWT, the ``aud``
claim in the JWT will be checked if it is equal to the ``audience`` field given
in the configuration.

See `RFC <https://tools.ietf.org/html/rfc7519#section-4.1.3>`__ for more details.

This field can be a string, or a list of strings.

Examples:

.. code-block:: json

   {
     "type": "RS512",
     "jwk_url": "https://......",
     "audience": "myapp-1234"
   }

or

.. code-block:: json

   {
     "type": "RS512",
     "jwk_url": "https://......",
     "audience": ["myapp-1234", "myapp-6789"]
   }


.. admonition:: Important!

   Certain JWT providers share JWKs between multiple tenants. They use the
   ``aud`` claim of JWT to specify the intended audience for the JWT. Setting
   the ``audience`` field in the Hasura JWT configuration will make sure that
   the ``aud`` claim from the JWT is also checked during verification. Not doing
   this check will allow JWTs issued for other tenants to be valid as well.

   In these cases, you **MUST** set the ``audience`` field to appropriate value.
   Failing to do is a major security vulnerability.


``issuer``
^^^^^^^^^^
This is an optional field. It takes a string value.

When this field is set, during the verification process of JWT, the ``iss``
claim in the JWT will be checked if it is equal to the ``issuer`` field given
in the configuration.

See `RFC <https://tools.ietf.org/html/rfc7519#section-4.1.1>`__ for more details.

Examples:

.. code-block:: json

   {
     "type": "RS512",
     "jwk_url": "https://......",
     "issuer": "https://my-auth-server.com"
   }

.. note::

   Certain providers require you to verify the ``iss`` claim on the JWT. To do
   that you can set this field to the appropriate value.



Examples
^^^^^^^^

HMAC-SHA based
++++++++++++++
Your auth server is using HMAC-SHA algorithms to sign JWTs, and is using a
256-bit key. In this case, the JWT config will look like:

.. code-block:: json

   {
     "type":"HS256",
     "key": "3EK6FD+o0+c7tzBNVfjpMkNDi2yARAAKzQlk8O2IKoxQu4nF7EdAh8s3TwpHwrdWT6R"
   }

The ``key`` is the actual shared secret, which is used by Hasura and the external auth server.

RSA based
+++++++++
If your auth server is using RSA to sign JWTs, and is using a 512-bit key. In this case,
the JWT config needs to have the only the public key.

**Example 1**: public key in PEM format (not OpenSSH format):

.. code-block:: json

    {
      "type":"RS512",
      "key": "-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDdlatRjRjogo3WojgGHFHYLugd\nUWAY9iR3fy4arWNA1KoS8kVw33cJibXr8bvwUAUparCwlvdbH6dvEOfou0/gCFQs\nHUfQrSDv+MuSUMAe8jzKE4qW+jK+xQU9a03GUnKHkkle+Q0pX/g6jXZ7r1/xAK5D\no2kQ+X5xK9cipRgEKwIDAQAB\n-----END PUBLIC KEY-----\n"
    }

**Example 2**: public key as X509 certificate:

.. code-block:: json

    {
      "type":"RS512",
      "key": "-----BEGIN CERTIFICATE-----\nMIIDHDCCAgSgAwIBAgIINw9gva8BPPIwDQYJKoZIhvcNAQEFBQAwMTEvMC0GA1UE\nAxMmc2VjdXJldG9rZW4uc3lzdGVtLmdzZXJ2aWNlYWNjb3VudC5jb20wHhcNMTgQt7dIsMTIU9k1SUrFviZOGnmHWtIAw\nmtYBcM9I0f9/ka45JIRp5Y1NKpAMFSShs7Wv0m1JS1kXQHdJsPSmjmDKcwnBe3R/\nTU3foRRywR/3AJRM15FNjTqvUm7TeaW16LkkRoECAwEAAaM4MDYwDAYDVR0TAQH/\nBAIwADAOBgNVHQ8BAf8EBAMCB4AwFgYDVR0lAQH/BAwwCgYIKwYBBQUHAwIwDQYJ\nKoZIhvcNAQEFBQADggEBADfY2DEmc2gb8/pqMNWHYq/nTYfJPpK4VA9A0lFTNeoq\nzmnbGwhKj24X+Nw8trsvkrKxHvCI1alDgBaCyzjGGvgOrh8X0wLtymp1yj6PWwee\nR2ZPdUaB62TCzO0iRv7W6o39ey+mU/FyYRtxF0ecxG2a0KNsIyFkciXUAeC5UVDo\nBNp678/SDDx9Ltuxc6h56a/hpBGf9Yzhr0RvYy3DmjBs6eopiGFmjnOKNxQrZ5t2\n339JWR+yiGEAtoHqk/fINMf1An6Rung1xYowrm4guhCIVi5unAvQ89fq0I6mzPg6\nLhTpeP0o+mVYrBmtYVpDpv0e71cfYowSJCCkod/9YbY=\n-----END CERTIFICATE-----"
    }

**Example 3**: public key published as JWKs:

.. code-block:: json

    {
      "type":"RS512",
      "jwk_url": "https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com"
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
      --admin-secret myadminsecretkey \
      --jwt-secret '{"type":"HS256", "key": "3EK6FD+o0+c7tzBNVfjpMkNDi2yARAAKzQlk8O2IKoxQu4nF7EdAh8s3TwpHwrdWT6R"}'

Using env vars:

.. code-block:: shell

  $ docker run -p 8080:8080 \
      -e HASURA_GRAPHQL_ADMIN_SECRET="myadminsecretkey" \
      -e HASURA_GRAPHQL_JWT_SECRET='{"type":"RS512", "key": "-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDdlatRjRjogo3WojgGHFHYLugd\nUWAY9iR3fy4arWNA1KoS8kVw33cJibXr8bvwUAUparCwlvdbH6dvEOfou0/gCFQs\nHUfQrSDv+MuSUMAe8jzKE4qW+jK+xQU9a03GUnKHkkle+Q0pX/g6jXZ7r1/xAK5D\no2kQ+X5xK9cipRgEKwIDAQAB\n-----END PUBLIC KEY-----\n"}' \
      hasura/graphql-engine:latest \
      graphql-engine \
      --database-url postgres://username:password@hostname:port/dbname \
      serve


Security considerations
-----------------------

Setting audience check
^^^^^^^^^^^^^^^^^^^^^^
Certain JWT providers share JWKs between multiple tenants (like Firebase). They use the ``aud`` claim of JWT to specify the intended tenant for the JWT. Setting the ``audience`` field in the Hasura JWT configuration will make sure that the ``aud`` claim from the JWT is also checked during verification. Not doing this check will allow JWTs issued for other tenants to be valid as well.

In these cases, you **MUST** set the ``audience`` field to appropriate value. Failing to do is a major security vulnerability.


Popular providers and known issues
----------------------------------

Firebase
^^^^^^^^
This page of Firebase `docs <https://firebase.google.com/docs/auth/admin/verify-id-tokens#verify_id_tokens_using_a_third-party_jwt_library>`__
mentions that JWKs are published under:

https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com .

But that is a non-standard format. Firebase also publishes the same certificates
as proper JWK format under:

https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com .

If you are using Firebase and Hasura, use this config:

.. code-block:: json

   {
     "type":"RS256",
     "jwk_url": "https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com",
     "audience": "<firebase-project-id>",
     "issuer": "https://securetoken.google.com/<firebase-project-id>"
   }


.. _auth0-issues:

Auth0
^^^^^

Refer the :doc:`Auth0 JWT Integration guide <../../guides/integrations/auth0-jwt>` for a full integration guide
with Auth0

Auth0 publishes their JWK under:

``https://<your-auth0-domain>.auth0.com/.well-known/jwks.json``

But they have a `bug where the certificate thumbprint does not match
<https://community.auth0.com/t/certificate-thumbprint-is-longer-than-20-bytes/7794/3>`__.
Hence, currently this URL does not work with Hasura.

Current workaround is - download the X590 certificate from:

``https://<your-auth0-domain>.auth0.com/pem``

And use it in the ``key`` field:

.. code-block:: json

        {
          "type":"RS512",
          "key": "-----BEGIN CERTIFICATE-----
    MIIDDTCAfWgAwIBAgIJhNlZ11IDrxbMA0GCSqSIb3DQEBCwUAMCQxIjAgBgNV
    BAMTGXlc3QtaGdlLWp3C5ldS5hdXRoMC5jb20HhcNMTgwNzMwMTM1MjM1WhcN
    MzIwND3MTM1MjM1WjAkSIwIAYDVQQDExl0ZXNLWhnZS1qd3QuZXUuYXV0aDAu
    Y29tMIBIjANBgkqhkiGw0BAQEFAAOCAQ8AMIICgKCAQEA13CivdSkNzRnOnR5
    ZNiReD+AgbL7BWjRiw3RwjxRp5PYzvAGuj94yR6LRh3QybYtsMFbSg5J7fNq6
    Ld6yMpMrUu8CBOnYY456b/2jlf+Vp8vEQuKvPOOw8Ev6x7X3blcuXCELSwyL3
    AGHq9OP2RV6V6CIE863zzuYH5HDLzU35oMZqogJVRJM0+6besH6TnSTNiA7xi
    BAqFaiRNQRVi1CAUa0bkN1XRp4AFy7d63VldOsM+8QnCNHySdDr1XevVuq6DK
    LQyGexFy4niALgHV0Q7A+xP1c2G6rJomZmn4j1avnlBpU87E58JMrRHOCj+5m
    Xj22/QDAQABo0IwQDAPgNVHRMBAf8EBTADAQHMB0GA1UdDgQWBBT6FvNkuUgu
    tk3OYQi4lo5aOgwazAOgNVHQ8BAf8EBAMCAoQDQYJKoZIhvcNAQELBQADggEB
    ADCLj+L22pEKyqaIUlhUJh7DAiDSLafy0fw56CntzPhqiZVVRlhxeAKidkCLV
    r9IEbRuxUoXiQSezPqM//9xHegMp0f2VauVCFg7EpUanYwvqFqjy9LWgH+SBz
    4uroLSZ5g1EPsHtlArLChA90caTX4e7Z7Xlu8G2kHRJB5nC7ycdbMUvEWBMeI
    tn/pcbmZ3/vlgj4UTEnURe2UPmSJpxmPwXqBcvwdKHRMgFXhZxojWCi0z4ftf
    f8t8UJIcbEblnkYe7wzYy8tOXoMMHqGSisCdkp/866029rJsKbwd8rVIyKNC5
    frGYaw+0cxO6/WvSir0eA=
    -----END CERTIFICATE-----
    "
        }

Generating JWT Config
---------------------

The JWT Config to be used in env ``HASURA_GRAPHQL_JWT_SECRET`` or ``--jwt-secret`` flag can be generated using:
https://hasura.io/jwt-config.

**Currently the UI supports generating config for Auth0 and Firebase**.

The config generated from this page can be directly pasted in yaml files and command line arguments as it takes
care of escaping new lines.

.. thumbnail:: ../../../../img/graphql/manual/auth/jwt-config-generated.png
   :width: 75%

Auth JWT Examples
-----------------

Here are some sample apps that use JWT authorization. You can follow the instructions in the READMEs of the
repositories to get started.

- `Auth0 JWT example <https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/todo-auth0-jwt>`__:
  A todo app that uses Hasura GraphQL Engine and Auth0 JWT

- `Firebase JWT example <https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/firebase-jwt>`__:
  Barebones example to show how to have Firebase Auth integrated with Hasura JWT mode
