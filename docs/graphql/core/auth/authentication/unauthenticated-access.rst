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

Once you have configured an :ref:`admin secret <securing_graphql_endpoint>`, by default Hasura GraphQL engine will reject any unauthenticated request it
receives. 

You can configure Hasura GraphQL engine to allow access to unauthenticated users by defining a specific role
which will be set for all unauthenticated requests. Once an unauthenticated role is configured, unauthenticated requests will 
not be rejected and instead the request will be made with the configured role.

This role can then be used to define the permissions for unauthenticated users as described in :ref:`authorization`.
A guide on setting up unauthenticated user permissions can be found :ref:`here <anonymous_users_example>`.

Configuring unauthenticated / public access
-------------------------------------------

An unauthenticated role can be configured for the following scenarios: no auth setup, webhook auth setup or JWT auth setup.

No auth setup
^^^^^^^^^^^^^

With no auth setup, every request is considered an unauthenticated request.

You can use the env variable ``HASURA_GRAPHQL_UNAUTHORIZED_ROLE`` or the ``--unauthorized-role`` flag to set a role
for unauthenticated (non-logged in) users. See :ref:`server_flag_reference` for more details
on setting this flag/env var.

Webhooks
^^^^^^^^

For :ref:`webhook authentication <auth_webhooks>`, any request for which the webhook returns a ``401 Unauthorized`` response 
is considered an unauthenticated request.

To allow unauthenticated access, the auth webhook should return a ``200`` status response with your unauthenticated role in the headers. For details on the webhook response, refer to :ref:`this section <webhook_response>`.

JWT
^^^

For :ref:`JWT authentication <auth_jwt>`, any request which does not contain a JWT token is considered an unauthenticated request.

You can use the env variable ``HASURA_GRAPHQL_UNAUTHORIZED_ROLE`` or the ``--unauthorized-role`` flag to set a role
for unauthenticated (non-logged in) users. See :ref:`server_flag_reference` for more details
on setting this flag/env var.
