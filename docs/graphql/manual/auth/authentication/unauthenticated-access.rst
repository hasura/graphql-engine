.. meta::
   :description: Manage unauthenticated / public access in Hasura
   :keywords: hasura, docs, authentication, auth, unauthenticated access, public access

.. _unauthenticated_access:

Unauthenticated / Public access
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

It is a common requirement to have requests which are accessible to all users without the need for any authentication
(logging in). For example, to display a public feed of events.

Once you have configured authentication, by default Hasura GraphQL engine will reject any unauthenticated request it
receives. 

You can configure Hasura GraphQL engine to allow access to unauthenticated users by defining a specific role
which will be set for all unauthenticated requests. Once an unauthenticated role is configured, unaunthenticated requests will 
not be rejected and instead the request will be made with the configured role.

This role can then be used to define the permissions for unauthenticated users as described in :ref:`authorization`.
A guide on setting up unauthenticated user permissions can be found :ref:`here <anonymous_users_example>`.

Configuring unauthenticated / public access
-------------------------------------------

Depending on your auth setup an unauthenticated role can be configured as follows:

Webhooks
^^^^^^^^

For :ref:`webhook authentication <auth_webhooks>`, an unauthenticated request is any request for which the webhook returns a ``401 Unauthorized`` response.

For unauthenticated access, you can return a ``200`` status response with your defined unauthenticated role, e.g: ``{ "x-hasura-role": "<anonymous-role>" }``.

JWT
^^^

For :ref:`JWT authentication <auth_jwt>`, an unauthenticated request is any request which does not contain a JWT token.

You can use the env variable ``HASURA_GRAPHQL_UNAUTHORIZED_ROLE`` or ``--unauthorized-role`` flag to set a role
for unauthenticated (non-logged in) users. See :ref:`server_flag_reference` for more details
on setting this flag/env var.

