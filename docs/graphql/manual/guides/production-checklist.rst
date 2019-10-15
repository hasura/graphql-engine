.. _production-checklist:

Production checklist
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Security
--------

In order to increase the security of your GraphQL engine, some steps are necessary.

Reverse proxy
^^^^^^^^^^^^^

You might want to set up Hasura behind a load balancer to improve performance, reliability and security.

Both nginx and Caddy work well with Hasura.

nginx
~~~~~

`Here <https://docs.nginx.com/nginx/admin-guide/web-server/reverse-proxy/>`_ is how to configure a nginx reverse proxy.

Caddy
~~~~~

`Here <https://caddyserver.com/docs/proxy>`_ is how to configure a Caddy proxy.

Admin secret
^^^^^^^^^^^^

Configure an admin secret like described :doc:`here <../deployment/securing-graphql-endpoint>`.

CORS domains
^^^^^^^^^^^^

Configure your CORS domains as described `here <https://docs.hasura.io/1.0/graphql/manual/deployment/graphql-engine-flags/config-examples.html#configure-cors>`_.

API security
------------

Allow-list
^^^^^^^^^^

White-list safe queries (GraphQL queries, mutations, subscriptions) by creating an :doc:`allow-list <../deployment/allow-list>`.

Limit rows
^^^^^^^^^^

Enable/disable API endpoints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Console availability
^^^^^^^^^^^^^^^^^^^^

Gateway
-------

Gzip responses
^^^^^^^^^^^^^^

If you are not using HTTP2, you can do :doc:`HTTP compression <../deployment/compression>` in order to increase speed.

Load balancing
^^^^^^^^^^^^^^

Logging
-------

Optimisations
-------------

Subscriptions
^^^^^^^^^^^^^