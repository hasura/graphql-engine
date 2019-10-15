.. _production-checklist:

Production checklist
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide is a checklist for configuring and securing GraphQL engine for a production deployment.

Security
--------

In order to increase the security of your GraphQL engine, the following steps are recommended.

Admin secret
^^^^^^^^^^^^

Configure an admin secret as described :doc:`here <../deployment/securing-graphql-endpoint>`.

Reverse proxy & API gateway
^^^^^^^^^^^^^^^^^^^^^^^^^^^

You might want to set up Hasura behind a load balancer to improve performance, reliability and security.

NOTE: SHAHIDH WILL ADD MORE INFO HERE INCLUDING CONFIGURATION

nginx
~~~~~

`Here <https://docs.nginx.com/nginx/admin-guide/web-server/reverse-proxy/>`_ is how to configure a nginx reverse proxy.

Caddy
~~~~~

`Here <https://caddyserver.com/docs/proxy>`_ is how to configure a Caddy proxy.


.. note::

  ``gzip`` compression is enabled by default if the client supports it.

CORS domains
^^^^^^^^^^^^

Configure your CORS domains as described `here <https://docs.hasura.io/1.0/graphql/manual/deployment/graphql-engine-flags/config-examples.html#configure-cors>`_.

API security
------------

Allow-list
^^^^^^^^^^

White-list safe queries (GraphQL queries, mutations, subscriptions) by creating an :doc:`allow-list <../deployment/allow-list>`.

Console availability
^^^^^^^^^^^^^^^^^^^^

It is recommended to `disable the console <https://docs.hasura.io/1.0/graphql/manual/migrations/existing-database.html#step-0-disable-console-on-the-server>`_ for the production instance.

Limit rows
^^^^^^^^^^

You can `limit the number of rows <https://docs.hasura.io/1.0/graphql/manual/auth/authorization/permission-rules.html#row-fetch-limit>`_ that can be accessed with one request.

Disable admin endpoints
^^^^^^^^^^^^^^^^^^^^^^^

It is recommended to `disable access to the schema / metadata API <https://docs.hasura.io/1.0/graphql/manual/api-reference/schema-metadata-api/index.html#disabling-schema-metadata-api>`_ for the production instance.

Logging
-------

:doc:`This page <../deployment/logging>` describes the different types of server logs, as well as how to access them.
