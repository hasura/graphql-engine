.. meta::
   :description: Spec for JWT authenticaton in Hasura
   :keywords: hasura, docs, authentication, auth, JWT, spec

.. _auth_jwt_spec:

The JWT spec
============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

When your auth server generates the JWT, the custom claims in the JWT **must contain**
the following:

1. A ``x-hasura-default-role`` field : indicating the default role of that user i.e. the role that will be
   used in case ``x-hasura-role`` header is not passed.
2. A ``x-hasura-allowed-roles`` field : a list of allowed roles for the user i.e. acceptable values of the
   ``x-hasura-role`` header. The ``x-hasura-default-role`` specified should be a member of this list.

The claims in the JWT can have other ``x-hasura-*`` fields where their values
can only be strings. You can use these ``x-hasura-*`` fields in your
permissions.

Now the JWT should be sent by the client to the Hasura GraphQL engine via the
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
specific claims have to be present. This value can be configured using
``claims_namespace`` or ``claims_namespace_path`` in the JWT
config while starting the server.

**Note**: ``x-hasura-default-role`` and ``x-hasura-allowed-roles`` are
mandatory, while the rest of them are optional.

.. note::

   All ``x-hasura-*`` values should be of type ``String``, they will be converted to the
   right type automatically.

The default role can be overridden by the ``x-hasura-role`` header, while making a
request.

.. code-block:: http

   POST /v1/graphql HTTP/1.1
   Authorization: Bearer eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWI...
   X-Hasura-Role: editor

   ...

How Hasura stores JWTs
----------------------

TODO:

- Explain how Hasura stores JWTs

- Explain how these can be modified
