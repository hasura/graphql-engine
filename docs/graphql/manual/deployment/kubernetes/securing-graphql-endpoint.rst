.. meta::
   :description: Secure Hasura GraphQL endpoint with Kubernetes deployment
   :keywords: hasura, docs, deployment, kubernetes, secure

.. _kubernetes_secure:

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

.. note::

  The ``HASURA_GRAPHQL_ADMIN_SECRET`` should never be passed from the client to the Hasura GraphQL engine as it would
  give the client full admin rights to your Hasura instance. See :ref:`auth` for information on
  setting up authentication.


(optional) Use the admin secret with the CLI
--------------------------------------------

If you're using the Hasura console from the CLI, you have two options to pass the admin secret.

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
