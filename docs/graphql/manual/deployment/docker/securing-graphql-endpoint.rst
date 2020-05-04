.. meta::
   :description: Secure Hasura GraphQL endpoint with Docker deployment
   :keywords: hasura, docs, deployment, docker, secure

.. _docker_secure:

Securing the GraphQL endpoint (Docker)
======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an admin secret key.

Run the Docker command with an admin-secret env var
---------------------------------------------------

.. code-block:: bash
   :emphasize-lines: 5

    #! /bin/bash
    docker run -d -p 8080:8080 \
     -e HASURA_GRAPHQL_DATABASE_URL=postgres://username:password@hostname:port/dbname \
     -e HASURA_GRAPHQL_ENABLE_CONSOLE=true \
     -e HASURA_GRAPHQL_ADMIN_SECRET=myadminsecretkey \
     hasura/graphql-engine:latest


.. note::

  The ``HASURA_GRAPHQL_ADMIN_SECRET`` should never be passed from the client to the Hasura GraphQL engine as it would
  give the client full admin rights to your Hasura instance. See :ref:`auth` for information on
  setting up authentication.

Use the admin secret with the CLI
---------------------------------

There are two options to pass the admin secret.

Pass admin secret as a flag
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   hasura console --admin-secret=myadminsecretkey

Set admin secret in the ``config.yaml``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: yaml
   :emphasize-lines: 2

   endpoint: https://your-app-url.com
   admin_secret: myadminsecretkey
