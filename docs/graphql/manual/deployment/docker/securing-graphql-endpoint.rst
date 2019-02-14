Securing the GraphQL endpoint (Docker)
======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an admin secret key.

Run the docker command with an admin-secret env var
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

  If you're looking at adding access control rules for your data to your GraphQL API then head
  to :doc:`Authentication / access control <../auth/index>`.
