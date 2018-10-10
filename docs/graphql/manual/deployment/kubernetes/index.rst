Run Hasura GraphQL Engine on Kubernetes
=======================================

This guide assumes that you already have Postgres running and helps you set up the Hasura GraphQL engine on Kubernetes
and connect it to your Postgres database.


Step 1: Get the Kubernetes deployment and service files
-------------------------------------------------------

The `hasura/graphql-engine/install-manifests <https://GitHub.com/hasura/graphql-engine/tree/master/install-manifests>`_ repo
contains all installation manifests required to deploy Hasura anywhere. Get the Kubernetes deployment and service files
from there:

.. code-block:: bash

   $ wget https://raw.githubusercontent.com/hasura/graphql-engine/master/install-manifests/kubernetes/deployment.yaml
   $ wget https://raw.githubusercontent.com/hasura/graphql-engine/master/install-manifests/kubernetes/svc.yaml

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

Examples of ``database-url``:

- ``postgres://admin:password@localhost:5432/my-db``
- ``postgres://admin:@localhost:5432/my-db`` *(if there is no password)*

Step 3: Create the Kubernetes deployment and service
----------------------------------------------------

.. code-block:: bash

  $ kubectl create -f deployment.yaml
  $ kubectl create -f svc.yaml

Step 4: Open the hasura console
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

See :doc:`../../schema/using-existing-database` to enable GraphQL over the database.

Advanced:
---------

- :doc:`Securing your GraphQL endpoint <securing-graphql-endpoint>`
- :doc:`GraphQL engine server logs <logging>`
- :doc:`Updating GraphQL engine <updating>`
- :doc:`Setting up migrations <../../migrations/index>`

.. toctree::
   :titlesonly:
   :hidden:

   Securing your GraphQL endpoint <securing-graphql-endpoint>
   GraphQL engine server logs <logging>
   Updating GraphQL engine <updating>
