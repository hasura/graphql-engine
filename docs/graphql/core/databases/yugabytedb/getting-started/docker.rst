.. meta::
  :description: Hasura with Docker for YugabyteDB
  :keywords: hasura, docs, databases, yugabytedb, docker

.. _database_yugabytedb_docker:

Get Started with Docker (Hasura & YugabyteDB)
=====================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Try it out
----------

Pre-requisites
^^^^^^^^^^^^^^

* `Docker <https://docs.docker.com/install/>`_
* `Docker Compose <https://docs.docker.com/compose/install/>`_
* A YugabyteDB cluster.
   - You can `sign up for Yugabyte Cloud <https://cloud.yugabyte.com/register/>`__ and get a free YugabyteDB cluster.
   - You can `install YugabyteDB <https://docs.yugabyte.com/latest/quick-start/install/>`__ on MacOS, Linux, Docker, or Kubernetes.

Step 1: Get the docker-compose file
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Get the Hasura docker-compose file:

.. code-block:: bash

   # in a new directory run
   wget https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/docker-compose/docker-compose.yaml
   # or run
   curl https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/docker-compose/docker-compose.yaml -o docker-compose.yaml

Step 2: Run Hasura GraphQL engine
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following command will run Hasura along with a PostgreSQL database required
for its functioning.

.. code-block::  bash

   $ docker-compose up -d

Check if the containers are running:

.. code-block:: bash

  $ docker ps

  CONTAINER ID IMAGE                 ... CREATED STATUS PORTS          ...
  097f58433a2b hasura/graphql-engine ... 1m ago  Up 1m  8080->8080/tcp ...
  b0b1aac0508d postgres              ... 1m ago  Up 1m  5432/tcp       ...

  Note that you will see a PostgreSQL database running, which is used by Hasura to store its configuration (Hasura metadata).

Step 3: Open the Hasura console
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Head to ``http://localhost:8080/console`` to open the Hasura console.

Step 4: Add your YugabyteDB database as a source to Hasura
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the ``Data > Data Manager > Connect Existing Database`` section on the console, 
select ``PostgreSQL`` from the ``Data Source Driver`` dropdown and add the connection string
directly or through an environment variable. As YugabyteDB uses same protocol as 
PostgreSQL, the connection string will start with ``postgres://``, i.e, there is no
difference between YugabyteDB’s connection strings and PostgreSQL’s connection strings.

.. thumbnail:: /img/graphql/core/databases/yugabytedb/4-add-source-docker.png
   :alt: Add source
   :width: 600px

If you're testing Hasura with YugabyteDB running locally, :ref:`read this guide<docker_networking>` on Docker 
networking in case you're not sure how to make sure that your YugabyteDB cluster is reachable from the Hasura 
docker container on Linux, Mac or Windows.


Once you add the database, you'll see your database pop up on the sidebar.

Step 5: Track existing tables or create new tables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you have existing tables, head to the database page by clicking on the database name on the sidebar. You should see a list of tables.

Track tables selectively or all of them so that Hasura can introspect the tables and create the corresponding GraphQL schema.

If you have foreign keys, you'll also see suggested relationships. Again, you can choose to track them selectively or all at once.

If you don't have existing tables, go ahead and add new tables and data and try out some queries, just like with a regular PostgreSQL database.

Step 6: Try out a GraphQL query
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Head to the ``GraphiQL`` tab in the console and try running a GraphQL query. Use the explorer sidebar on GraphQL to get help in creating a GraphQL query.

.. thumbnail:: /img/graphql/core/databases/yugabytedb/3-make-graphql-query.png
   :alt: Make GraphQL query
   :width: 1000px
