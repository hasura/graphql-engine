.. meta::
   :description: Hasura Cloud multiple admin secrets
   :keywords: hasura, docs, cloud, security, allow, rotating, multiple, admin, secrets

.. _rotating_admin_secrets:

Rotating Admin Secrets
===========

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

You can specify a list of admin secrets which can be used to implement security mechanisms like rotating admin secrets.

How to use multiple admin secrets
---------------------------------

Multiple admin secrets can be provided in the env var ``HASURA_GRAPHQL_ADMIN_SECRETS`` which takes a JSON array of admin secrets (strings).

When you launch the console from the Hasura Cloud dashboard, you can use any secret from the admin secrets list to authenticate yourself as an admin.
If you want to make API calls from outside the console, you need to pass any one of the admin secrets as the `x-hasura-admin-secret` request header.

.. note::

    Only one of ``HASURA_GRAPHQL_ADMIN_SECRET`` or ``HASURA_GRAPHQL_ADMIN_SECRETS`` can be set at the same time. If both are set, then it will throw a configuration error at startup. 

How to rotate an admin secret
-----------------------------------

To implement a secret rotation mechanism, the following can be done:

1. Add a new secret to the list of admin secrets (and perform a rolling deploy)

2. Update applications/services using the old admin secret to use the new secret

3. Remove the old secret from the admin secret list (and perform a rolling deploy)