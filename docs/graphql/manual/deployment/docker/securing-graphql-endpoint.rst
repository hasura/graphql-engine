Securing the GraphQL endpoint (Docker)
======================================

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an access key.

Run the docker command with an access-key flag
----------------------------------------------

.. code-block:: bash
   :emphasize-lines: 7

    #! /bin/bash
    docker run -p 8080:8080 \
     hasura/graphql-engine:latest \
     graphql-engine \
     --database-url postgres://username:password@hostname:port/dbname \
     serve \
     --access-key mysecretkey


.. note::

  If you're looking at adding authentication and access control to your GraphQL API then head
  to :doc:`Authentication / access control <../../auth/index>`.
