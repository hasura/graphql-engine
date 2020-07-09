.. meta::
   :description: Deploy Hasura GraphQL engine with Kubernetes
   :keywords: hasura, docs, deployment, kubernetes

.. _deploy_kubernetes:

Run Hasura GraphQL engine on Kubernetes
=======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide assumes that you already have Postgres running and helps you set up the Hasura GraphQL engine on Kubernetes
and connect it to your Postgres database.


Step 1: Get the Kubernetes deployment and service files
-------------------------------------------------------

The `hasura/graphql-engine/install-manifests <https://github.com/hasura/graphql-engine/tree/stable/install-manifests>`_ repo
contains all installation manifests required to deploy Hasura anywhere. Get the Kubernetes deployment and service files
from there:

.. code-block:: bash

   $ wget https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/kubernetes/deployment.yaml
   $ wget https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/kubernetes/svc.yaml

Step 2: Set the Postgres database url
-------------------------------------

Edit ``deployment.yaml`` and set the right database url:

.. code-block:: yaml
  :emphasize-lines: 4

  ...
    env:
    - name: HASURA_GRAPHQL_DATABASE_URL
      value: postgres://username:password@hostname:port/dbname
  ...

Examples of ``HASURA_GRAPHQL_DATABASE_URL``:

- ``postgres://admin:password@localhost:5432/my-db``
- ``postgres://admin:@localhost:5432/my-db`` *(if there is no password)*

.. note::

  - If your **password contains special characters** (e.g. #, %, $, @, etc.), you need to URL encode them in the
    ``HASURA_GRAPHQL_DATABASE_URL`` env var (e.g. %40 for @).

    You can check the :ref:`logs <kubernetes_logs>` to see if the database credentials are proper and if Hasura is able
    to connect to the database.

  - The Hasura GraphQL engine needs access permissions on your Postgres database as described in
    :ref:`Postgres permissions <postgres_permissions>`.


Step 3: Create the Kubernetes deployment and service
----------------------------------------------------

.. code-block:: bash

  $ kubectl create -f deployment.yaml
  $ kubectl create -f svc.yaml

Step 4: Open the Hasura console
-------------------------------

The above creates a LoadBalancer type service with port 80. So you should be able to access the console at the
external IP.

For example, using Docker-for-desktop on Mac:

.. code-block:: bash

  $ kubectl get svc
  NAME         TYPE           CLUSTER-IP      EXTERNAL-IP   PORT(S)        AGE
  hasura       LoadBalancer   10.96.214.240   localhost     80:30303/TCP   4m
  kubernetes   ClusterIP      10.96.0.1       <none>        443/TCP        8m

Head to: http://localhost and the console should load!

Step 5: Track existing tables and relationships
-----------------------------------------------

See :ref:`schema_existing_db` to enable GraphQL over the database.

Advanced
--------

- :ref:`Securing your GraphQL endpoint <kubernetes_secure>`
- :ref:`GraphQL engine server logs <kubernetes_logs>`
- :ref:`Updating GraphQL engine <kubernetes_update>`
- :ref:`Setting up migrations <migrations>`

