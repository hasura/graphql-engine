Securing the GraphQL endpoint (Kubernetes)
==========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an access key.


Add the HASURA_GRAPHQL_ACCESS_KEY env var
-----------------------------------------

Update the ``deployment.yaml`` to set the ``HASURA_GRAPHQL_ACCESS_KEY`` environment variable.

.. code-block:: yaml
  :emphasize-lines: 10,11

   ...
   spec:
      containers:
        ...
        command: ["graphql-engine"]
        args: ["serve", "--enable-console"]
        env:
        - name: HASURA_GRAPHQL_DATABASE_URL
          value: postgres://username:password@hostname:port/dbname
        - name: HASURA_GRAPHQL_ACCESS_KEY
          value: mysecretkey
        ports:
        - containerPort: 8080
          protocol: TCP
        resources: {}



(optional) Use the access key with the CLI
------------------------------------------

In case you're using the CLI to open the Hasura console, use the ``access-key`` flag when you open the console:

.. code-block:: bash

   hasura console --access-key=mysecretkey


.. note::

  If you're looking at adding authentication and access control to your GraphQL API then head
  to :doc:`Authentication / access control <../../auth/index>`.