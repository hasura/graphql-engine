.. meta::
   :description: Configure authenticaton with JWTs in Hasura
   :keywords: hasura, docs, authentication, auth, JWT, configure

.. _configure_auth_jwt:

Configuring JWT mode
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This page explains how to configure JWT mode in Hasura.

Configuring JWT mode
--------------------

You can enable JWT mode by using the ``--jwt-secret`` flag or
``HASURA_GRAPHQL_JWT_SECRET`` environment variable; the value of which is a
JSON object:

.. code-block:: none

   {
     "type": "<optional-type-of-key>",
     "key": "<optional-key-as-string>",
     "jwk_url": "<optional-url-to-refresh-jwks>",
     "claims_namespace": "<optional-key-name-in-claims>",
     "claims_namespace_path":"<optional-json-path-to-the-claims>",
     "claims_format": "json|stringified_json",
     "audience": <optional-string-or-list-of-strings-to-verify-audience>,
     "issuer": "<optional-string-to-verify-issuer>"
   }

(``type``, ``key``) pair or ``jwk_url``, **one of them has to be present**.

``type``
^^^^^^^^
Valid values are : ``HS256``, ``HS384``, ``HS512``, ``RS256``,
``RS384``, ``RS512``. (see https://jwt.io).

``HS*`` is for HMAC-SHA based algorithms. ``RS*`` is for RSA based signing. For
example, if your auth server is using HMAC-SHA256 for signing the JWTs, then
use ``HS256``. If it is using RSA with 512-bit keys, then use ``RS512``. EC
public keys are not yet supported.

This is an optional field. This is required only if you are using ``key`` in the config.

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
https://tools.ietf.org/html/rfc7517.

This is an optional field. You can also provide the key (certificate, PEM
encoded public key) as a string - in the ``key`` field along with the ``type``.

Rotating JWKs
+++++++++++++

Some providers rotate their JWKs (e.g. Firebase). If the provider sends

1. ``max-age`` or ``s-maxage`` in ``Cache-Control`` header
2. or ``Expires`` header

with the response of JWK, then the GraphQL engine will refresh the JWKs automatically. If the
provider does not send the above, the JWKs are not refreshed.

Following is the behaviour in detail:

**On startup**:

1. GraphQL engine will fetch the JWK and will -

   1. first, try to parse ``max-age`` or ``s-maxage`` directive in ``Cache-Control`` header.
   2. second, check if ``Expires`` header is present (if ``Cache-Control`` is not present), and try
      to parse the value as a timestamp.

2. If it is able to parse any of the above successfully, then it will use that parsed time to
   refresh/refetch the JWKs again. If it is unable to parse, then it will not refresh the JWKs (it
   assumes that if the above headers are not present, the provider doesn't rotate their JWKs).

**While running**:

1. While GraphQL engine is running with refreshing JWKs, in one of the refresh cycles it will -

   1. first, try to parse ``max-age`` or ``s-maxage`` directive in ``Cache-Control`` header.
   2. second, check if ``Expires`` header is present (if ``Cache-Control`` is not present), and try
      to parse the value as a timestamp.

2. If it is able to parse any of the above successfully, then it will use that parsed time to
   refresh/refetch the JWKs again. If it is unable to parse, then it will sleep for 1 minute and
   will start another refresh cycle.

Example JWK URL
+++++++++++++++

- Auth0 publishes their JWK url at: ``https://<YOUR_AUTH0_DOMAIN>.auth0.com``.
  But Auth0 has a bug. See known issues: :ref:`auth0-issues`.
- Firebase publishes their JWK url at:
  ``https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com``.

``claims_namespace``
^^^^^^^^^^^^^^^^^^^^
This is an optional field. You can specify the key name,
inside which the Hasura specific claims will be present, e.g. ``https://mydomain.com/claims``.

**Default value** is: ``https://hasura.io/jwt/claims``.

``claims_namespace_path``
^^^^^^^^^^^^^^^^^^^^^^^^^
An optional JSON path value to the Hasura claims in the JWT token.

Example values are ``$.hasura.claims`` or ``$`` (i.e. root of the payload)

The JWT token should be in this format if the ``claims_namespace_path`` is
set to ``$.hasura.claims``:

.. code-block:: json

  {
    "sub": "1234567890",
    "name": "John Doe",
    "admin": true,
    "iat": 1516239022,
    "hasura": {
       "claims": {
          "x-hasura-allowed-roles": ["editor","user", "mod"],
          "x-hasura-default-role": "user",
          "x-hasura-user-id": "1234567890",
          "x-hasura-org-id": "123",
          "x-hasura-custom": "custom-value"
       }
     }
  }

.. note::

   The JWT config can only have one of ``claims_namespace`` or ``claims_namespace_path``
   values set. If neither keys are set, then the default value of
   ``claims_namespace`` i.e. https://hasura.io/jwt/claims will be used.

``claims_format``
^^^^^^^^^^^^^^^^^

This is an optional field, with only the following possible values:
- ``json``
- ``stringified_json``

Default is ``json``.

This is to indicate whether the Hasura specific claims are a regular JSON object
or a stringified JSON.

This is required because providers like AWS Cognito only allow strings in the
JWT claims. `See #1176 <https://github.com/hasura/graphql-engine/issues/1176>`__.

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
     "jwk_url": "https://......",
     "audience": "myapp-1234"
   }

or

.. code-block:: json

   {
     "jwk_url": "https://......",
     "audience": ["myapp-1234", "myapp-6789"]
   }


.. admonition:: Important!

   Certain JWT providers share JWKs between multiple tenants. They use the
   ``aud`` claim of JWT to specify the intended audience for the JWT. Setting
   the ``audience`` field in the Hasura JWT configuration will make sure that
   the ``aud`` claim from the JWT is also checked during verification. Not doing
   this check will allow JWTs issued for other tenants to be valid as well.

   In these cases, you **MUST** set the ``audience`` field to the appropriate value.
   Failing to do so is a major security vulnerability.


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
     "jwk_url": "https://......",
     "issuer": "https://my-auth-server.com"
   }

.. note::

   Certain providers require you to verify the ``iss`` claim on the JWT. To do
   that you can set this field to the appropriate value.

Configuring custom claims
-------------------------

TODO

JWT expiry
----------

TODO

Running with JWT
----------------

Using the flag
^^^^^^^^^^^^^^

.. code-block:: shell

  $ docker run -p 8080:8080 \
      hasura/graphql-engine:latest \
      graphql-engine \
      --database-url postgres://username:password@hostname:port/dbname \
      serve \
      --admin-secret myadminsecretkey \
      --jwt-secret '{"type":"HS256", "key": "3EK6FD+o0+c7tzBNVfjpMkNDi2yARAAKzQlk8O2IKoxQu4nF7EdAh8s3TwpHwrdWT6R"}'

Using env vars
^^^^^^^^^^^^^^

.. code-block:: shell

  $ docker run -p 8080:8080 \
      -e HASURA_GRAPHQL_ADMIN_SECRET="myadminsecretkey" \
      -e HASURA_GRAPHQL_JWT_SECRET='{"type":"RS512", "key": "-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDdlatRjRjogo3WojgGHFHYLugd\nUWAY9iR3fy4arWNA1KoS8kVw33cJibXr8bvwUAUparCwlvdbH6dvEOfou0/gCFQs\nHUfQrSDv+MuSUMAe8jzKE4qW+jK+xQU9a03GUnKHkkle+Q0pX/g6jXZ7r1/xAK5D\no2kQ+X5xK9cipRgEKwIDAQAB\n-----END PUBLIC KEY-----\n"}' \
      hasura/graphql-engine:latest \
      graphql-engine \
      --database-url postgres://username:password@hostname:port/dbname \
      serve

JWTs & local development
------------------------

TODO

Examples
--------

HMAC-SHA based
^^^^^^^^^^^^^^

Your auth server is using HMAC-SHA algorithms to sign JWTs, and is using a
256-bit key. In this case, the JWT config will look like:

.. code-block:: json

   {
     "type":"HS256",
     "key": "3EK6FD+o0+c7tzBNVfjpMkNDi2yARAAKzQlk8O2IKoxQu4nF7EdAh8s3TwpHwrdWT6R"
   }

The ``key`` is the actual shared secret, which is used by Hasura and the external auth server.

RSA based
^^^^^^^^^

If your auth server is using RSA to sign JWTs, and is using a 512-bit key,
the JWT config only needs to have the public key.

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
      "jwk_url": "https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com"
    }




