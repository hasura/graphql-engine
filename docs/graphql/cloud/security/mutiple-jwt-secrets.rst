.. meta::
   :description: Hasura Cloud multiple JWT Secrets
   :keywords: hasura, docs, cloud, security, allow, , multiple, JWT, secrets

.. _multiple_jwt_secrets:

Multiple JWT Secrets
===========

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

You can configure Hasura with a list of JWT Secrets so that you can integrate with different JWT issuers. This is useful when you have different authentication providers using the same Hasura infrastructure.

How to use multiple jwt secrets
---------------------------------

Multiple JWT secrets can be provided in the env var ``HASURA_GRAPHQL_JWT_SECRETS`` which takes a JSON array of JWT secret objects.

A single JWT secret can be provided without an `issuer`. Providing multiple secrets without an `issuer` will result in a configuration error at startup.

Bearer Tokens are authenticated against the secret with a matching `issuer`.

The authentication is resolved as follows:

1. The `issuer` is decoded from the bearer token.
2. A JWT secret is looked up by `issuer` in the JWT secrets array.
3. If a secret with a matching `issuer` is found then authenticate the token against that secret.
4. If no secret is found or if the bearer token contains no `issuer` then verify against
   the `no-issuer` secret.
5. If there is not a `no-issuer` secret then return an auth failure.

.. note::
   Authentication resolution is unchanged when using a single JWT secret.

.. note::

    Only one of ``HASURA_GRAPHQL_JWT_SECRET`` or ``HASURA_GRAPHQL_JWT_SECRETS`` can be set at the same time. If both are set, then it will throw a configuration error at startup.
