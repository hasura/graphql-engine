Authentication
==============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Overview
--------

Authentication is handled outside Hasura. Hasura delegates authentication and resolution of request
headers into session variables to your existing (*or new/custom*) authentication service.

Authentication options
----------------------

Hasura supports two modes of authentication configuration:

1) **Webhook**: Your auth server exposes a webhook that is used to authenticate all incoming requests
   to the Hasura GraphQL engine server and to get metadata about the request to evaluate access control
   rules. Here's how a GraphQL request is processed in Webhook mode:

   .. thumbnail:: ../../../../img/graphql/manual/auth/auth-webhook-overview.png

2) **JWT** (JSON Web Token): Your auth server issues JWTs to your client app, which, when sent as part
   of the request, are verified and decoded by GraphQL engine to get metadata about the request to
   evaluate access control rules. Here's how a GraphQL query is processed in JWT mode:

   .. thumbnail:: ../../../../img/graphql/manual/auth/auth-jwt-overview.png

**See more details about these at:**

.. toctree::
  :maxdepth: 1

  Using webhooks <webhook>
  Using JWT <jwt>
