Unauthenticated access
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Use case
--------

It is a typical requirement to have requests which are accessible to all users without the need for any authentication
(logging in). For example, to display a public feed of events.

You can configure Hasura GraphQL engine to allow access to unauthenticated users by defining a specific role
which will be set for all unauthenticated requests.

Configuring unauthenticated access
----------------------------------

You can use the env variable ``HASURA_GRAPHQL_UNAUTHORIZED_ROLE`` or ``--unauthorized-role`` flag to set a role
for unauthenticated (non-logged in) users. The configured unauthorized role will be used whenever an access token is not present
in a request to the GraphQL API. See :doc:`../../deployment/graphql-engine-flags/reference` for more details on setting this flag/env var.

This role can then be used to define the permissions for unauthenticated users as described in :doc:`../authorization/index`.
A guide on setting up unauthenticated user permissions can be found at :ref:`anonymous_users_example`
