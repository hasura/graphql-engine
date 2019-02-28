Securing the GraphQL endpoint (Kubernetes)
==========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an admin secret key.


Add the HASURA_GRAPHQL_ADMIN_SECRET env var
-------------------------------------------

Update the ``deployment.yaml`` to set the ``HASURA_GRAPHQL_ADMIN_SECRET`` environment variable.

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
        - name: HASURA_GRAPHQL_ADMIN_SECRET
          value: mysecretkey
        ports:
        - containerPort: 8080
          protocol: TCP
        resources: {}



(optional) Use the admin secret key with the CLI
------------------------------------------------

In case you're using the CLI to open the Hasura console, use the ``admin-secret`` flag when you open the console:

.. code-block:: bash

   hasura console --admin-secret=myadminsecretkey


.. note::

  If you're looking at adding access control rules for your data to your GraphQL API then head
  to :doc:`Authentication / access control <../../auth/index>`.