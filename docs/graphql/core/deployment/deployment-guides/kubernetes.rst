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

Introduction
------------

This guide assumes that you already have Postgres running and helps you set up the Hasura GraphQL engine on Kubernetes
and connect it to your Postgres database.

Deploying Hasura using Kubernetes
---------------------------------

Step 1: Get the Kubernetes deployment and service files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The `hasura/graphql-engine/install-manifests <https://github.com/hasura/graphql-engine/tree/stable/install-manifests>`__ repo
contains all installation manifests required to deploy Hasura anywhere. Get the Kubernetes deployment and service files
from there:

.. code-block:: bash

   $ wget https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/kubernetes/deployment.yaml
   $ wget https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/kubernetes/svc.yaml

Step 2: Set the Postgres database url
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

  $ kubectl create -f deployment.yaml
  $ kubectl create -f svc.yaml

Step 4: Open the Hasura console
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

See :ref:`schema_existing_db` to enable GraphQL over the database.

.. _kubernetes_secure:

Securing the GraphQL endpoint
-----------------------------

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an admin secret key.


Add the HASURA_GRAPHQL_ADMIN_SECRET env var
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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


(optional) Use the admin secret key with the CLI
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In case you're using the CLI to open the Hasura console, use the ``admin-secret`` flag when you open the console:

.. code-block:: bash

   hasura console --admin-secret=myadminsecretkey

.. _kubernetes_logs:

Hasura GraphQL engine server logs
---------------------------------

You can check the logs of the Hasura GraphQL engine deployed on Kubernetes by checking the logs of the GraphQL engine
service, i.e. ``hasura``:

.. code-block:: bash

  $ kubectl logs -f svc/hasura

  {"timestamp":"2018-10-09T11:20:32.054+0000", "level":"info", "type":"http-log", "detail":{"status":200, "query_hash":"01640c6dd131826cff44308111ed40d7fbd1cbed", "http_version":"HTTP/1.1", "query_execution_time":3.0177627e-2, "request_id":null, "url":"/v1/graphql", "user":{"x-hasura-role":"admin"}, "ip":"127.0.0.1", "response_size":209329, "method":"POST", "detail":null}}
  ...


**See:**

- https://kubernetes.io/docs/concepts/cluster-administration/logging for more details on logging in Kubernetes.

- :ref:`hge_logs` for more details on Hasura logs

.. _kubernetes_update:

Updating Hasura GraphQL engine
------------------------------

This guide will help you update the Hasura GraphQL engine running on Kubernetes. This guide assumes that you already have
the Hasura GraphQL engine running on Kubernetes.

Step 1: Check the latest release version
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The current latest version is:

.. raw:: html

   <code>hasura/graphql-engine:<span class="latest-release-tag">latest</span></code>

All the versions can be found at: https://github.com/hasura/graphql-engine/releases.

Step 2: Update the container image
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the ``deployment.yaml`` file, update the image tag to this latest version.

For example, if you had:

.. raw:: html

   <code>
     containers:<br>
       - image: hasura/graphql-engine:v1.0.0-alpha01
   </code>

you should change it to:

.. raw:: html

   <code>
     containers:<br>
       - image: hasura/graphql-engine:<span class="latest-release-tag">latest</span>
   </code>

Step 3: Rollout the change
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

  $ kubectl replace -f deployment.yaml


.. note::

  If you are downgrading to an older version of the GraphQL engine you might need to downgrade your metadata catalogue version
  as described in :ref:`downgrade_hge`

Advanced
--------

- :ref:`Setting up migrations <migrations>`

