.. meta::
   :description: Use authenticaton with JWT in Hasura
   :keywords: hasura, docs, authentication, auth, JWT

.. _auth_jwt:

Authentication using JWT
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

You can configure the GraphQL engine to use JWT authorization mode to authorize all incoming requests to the Hasura GraphQL engine server.

The idea is that your auth server will return JWT tokens, which are decoded and
verified by the GraphQL engine, to authorize and get metadata about the request
(``x-hasura-*`` values).

.. thumbnail:: /img/graphql/core/auth/jwt-auth.png
   :alt: Authentication using JWT

The JWT is decoded, the signature is verified, then it is asserted that the
requested role of the user (if specified in the request) is in the list of allowed roles.
If the desired role is not specified in the request, then the default role is applied.
If the authorization passes, then all of the ``x-hasura-*`` values in the claim
are used for the permissions system.

.. admonition:: Prerequisite

   It is mandatory to first :ref:`secure your GraphQL endpoint <securing_graphql_endpoint>` for the JWT mode to take effect.


In JWT mode, on a secured endpoint:

- JWT authentication is **enforced** when the ``X-Hasura-Admin-Secret`` header is **not found** in the request.
- JWT authentication is **skipped** when the ``X-Hasura-Admin-Secret`` header **is found** in the request and
  admin access is granted.


TL;DR
-----

1. The JWT must contain: ``x-hasura-default-role``, ``x-hasura-allowed-roles``
   in a custom namespace in the claims.
2. Other optional ``x-hasura-*`` fields (required as per your defined
   permissions).
3. You can send ``x-hasura-role`` as header in the request to indicate a role.
4. Send the JWT via ``Authorization: Bearer <JWT>`` header.


Security considerations
-----------------------

Setting audience check
^^^^^^^^^^^^^^^^^^^^^^
Certain JWT providers share JWKs between multiple tenants (like Firebase). They use the ``aud`` claim of JWT to specify the intended tenant for the JWT. Setting the ``audience`` field in the Hasura JWT configuration will make sure that the ``aud`` claim from the JWT is also checked during verification. Not doing this check will allow JWTs issued for other tenants to be valid as well.

In these cases, you **MUST** set the ``audience`` field to appropriate value. Failing to do so is a major security vulnerability.


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
     "jwk_url": "https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com",
     "audience": "<firebase-project-id>",
     "issuer": "https://securetoken.google.com/<firebase-project-id>"
   }


.. _auth0-issues:

Auth0
^^^^^

Refer the :ref:`Auth0 JWT Integration guide <guides_auth0_jwt>` for a full integration guide
with Auth0.

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

.. thumbnail:: /img/graphql/core/auth/jwt-config-generated.png
   :width: 75%
   :alt: Generating JWT config

Auth JWT Examples
-----------------

Here are some sample apps that use JWT authorization. You can follow the instructions in the READMEs of the
repositories to get started.

- `Auth0 JWT example <https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/todo-auth0-jwt>`__:
  A todo app that uses Hasura GraphQL engine and Auth0 JWT

- `Firebase JWT example <https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/firebase-jwt>`__:
  Barebones example to show how to have Firebase Auth integrated with Hasura JWT mode

Integrating webhook authentication
----------------------------------

.. toctree::
  :maxdepth: 1

  JWT spec <spec>
  Configure JWT mode <configure>
  Advanced use cases <advanced>
  Integration guides <guides/index>
