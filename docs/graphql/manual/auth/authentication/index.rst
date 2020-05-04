.. meta::
   :description: Manage authenticaton with Hasura
   :keywords: hasura, docs, authentication, auth

.. _authentication:

Authentication
==============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Overview
--------

Authentication is handled outside of Hasura. Hasura delegates authentication and resolution of request
headers into session variables to your authentication service *(existing or new)*.

Your authentication service is required to pass a user's **role** information in the form of session
variables like ``X-Hasura-Role``, etc. More often than not, you'll also need to pass user information
for your access control use cases, like ``X-Hasura-User-Id``, to build permission rules.

You can also configure Hasura to allow access to unauthenticated users by configuring a specific role
which will be set for all unauthenticated requests.

Authentication options
----------------------

Hasura supports two modes of authentication configuration:

1. Webhook
^^^^^^^^^^

Your auth server exposes a webhook that is used to authenticate all incoming requests
to the Hasura GraphQL engine server and to get metadata about the request to evaluate access control
rules.

Here's how a GraphQL request is processed in webhook mode:

.. thumbnail:: ../../../../img/graphql/manual/auth/auth-webhook-overview.png
   :alt: Authentication using webhooks

2. JWT (JSON Web Token)
^^^^^^^^^^^^^^^^^^^^^^^

Your auth server issues JWTs to your client app, which, when sent as part
of the request, are verified and decoded by the GraphQL engine to get metadata about the request to
evaluate access control rules.

Here's how a GraphQL query is processed in JWT mode:

.. thumbnail:: ../../../../img/graphql/manual/auth/auth-jwt-overview.png
   :alt: Authentication using JWT

3. Admin secret
^^^^^^^^^^^^^^^

An admin secret is added to prevent admin access for the Hasura console through third parties.

**See more details at:**

.. toctree::
  :maxdepth: 1

  Using webhooks <webhook>
  Using JWT <jwt>
  Using admin secret <admin-secret>
  Unauthenticated access <unauthenticated-access>
