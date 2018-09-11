Run Hasura GraphQL Engine on Kubernetes
=======================================

This guide assumes that you already have Postgres running and helps you set up the Hasura GraphQL engine on Kubernetes
and connect it to your Postgres database.


Step 1: Get the Kubernetes deployment and service files
-------------------------------------------------------

The `hasura/graphql-engine-install-manifests <https://github.com/hasura/graphql-engine-install-manifests>`_ repo
contains all installation manifests required to deploy Hasura anywhere. Get the Kubernetes deployment and service files
from there:

.. code-block:: bash

   $ wget https://raw.githubusercontent.com/hasura/graphql-engine-install-manifests/master/kubernetes/deployment.yaml
   $ wget https://raw.githubusercontent.com/hasura/graphql-engine-install-manifests/master/kubernetes/svc.yaml

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

For example, using docker-for-desktop on mac:

.. code-block:: bash

  $ kubectl get svc
  NAME         TYPE           CLUSTER-IP      EXTERNAL-IP   PORT(S)        AGE
  hasura       LoadBalancer   10.96.214.240   localhost     80:30303/TCP   4m
  kubernetes   ClusterIP      10.96.0.1       <none>        443/TCP        8m

Head to: http://localhost and the console should load!

Step 5: Track existing tables and relationships
-----------------------------------------------

On the console page, you'll see your existing tables/views as "Untracked tables/views" in the console. Click the
``Add all`` button to enable GraphQL APIs over them.

.. image:: ../../../../img/graphql/manual/getting-started/TrackTable.jpg

Advanced:
---------

- :doc:`Securing your GraphQL endpoint <securing-graphql-endpoint>`
- :doc:`Updating GraphQL engine <updating>`
- :doc:`Setting up migrations <../../migrations/index>`

.. toctree::
   :titlesonly:
   :hidden:

   Securing your GraphQL endpoint <securing-graphql-endpoint>
   Updating GraphQL engine <updating>
