.. meta::
   :description: Deploy Hasura GraphQL engine with Docker
   :keywords: hasura, docs, deployment, docker

.. _deployment_docker:

Run Hasura GraphQL engine using Docker
======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide will help you deploy the Hasura GraphQL engine and a Postgres database to store its metadata
using Docker Compose.

Deploying Hasura using Docker
-----------------------------

Prerequisites
^^^^^^^^^^^^^

- `Docker <https://docs.docker.com/install/>`_
- `Docker Compose <https://docs.docker.com/compose/install/>`__

Step 1: Get the docker-compose file
-----------------------------------

The `hasura/graphql-engine/install-manifests <https://github.com/hasura/graphql-engine/tree/stable/install-manifests>`__ repo
contains all installation manifests required to deploy Hasura anywhere. Get the docker compose file from there:

.. code-block:: bash

   # in a new directory run
   wget https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/docker-compose/docker-compose.yaml
   # or run
   curl https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/docker-compose/docker-compose.yaml -o docker-compose.yml

Step 2: Run Hasura GraphQL engine
---------------------------------

The following command will run Hasura GraphQL engine along with a Postgres database to store its metadata.

.. code-block::  bash

   $ docker-compose up -d

Check if the containers are running:

.. code-block:: bash

  $ docker ps

  CONTAINER ID IMAGE                 ... CREATED STATUS PORTS          ...
  097f58433a2b hasura/graphql-engine ... 1m ago  Up 1m  8080->8080/tcp ...
  b0b1aac0508d postgres              ... 1m ago  Up 1m  5432/tcp       ...

.. _docker_secure:

Securing the GraphQL endpoint
-----------------------------

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an admin secret key.

Run the Docker container with an admin-secret env var
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: yaml
   :emphasize-lines: 5

   graphql-engine:
     image: hasura/graphql-engine:v2.0.0
     environment:
       HASURA_GRAPHQL_METADATA_DATABASE_URL: postgres://postgres:postgrespassword@postgres:5432/postgres
       HASURA_GRAPHQL_ADMIN_SECRET: myadminsecretkey
     ...

.. note::

  The ``HASURA_GRAPHQL_ADMIN_SECRET`` should never be passed from the client to the Hasura GraphQL engine as it would
  give the client full admin rights to your Hasura instance. See :ref:`auth` for information on
  setting up authentication.

.. _docker_logs:

Hasura GraphQL engine server logs
---------------------------------

You can check the logs of the Hasura GraphQL engine deployed using Docker by checking the logs of the
GraphQL engine container:

.. code-block:: bash

  $ docker ps

  CONTAINER ID IMAGE                 ... CREATED STATUS PORTS          ...
  097f58433a2b hasura/graphql-engine ... 1m ago  Up 1m  8080->8080/tcp ...
  b0b1aac0508d postgres              ... 1m ago  Up 1m  5432/tcp       ...


  $ docker logs 097f58433a2b

  {"timestamp":"2018-10-09T11:20:32.054+0000", "level":"info", "type":"http-log", "detail":{"status":200, "query_hash":"01640c6dd131826cff44308111ed40d7fbd1cbed", "http_version":"HTTP/1.1", "query_execution_time":3.0177627e-2, "request_id":null, "url":"/v1/graphql", "user":{"x-hasura-role":"admin"}, "ip":"127.0.0.1", "response_size":209329, "method":"POST", "detail":null}}
  ...

**See:**

- https://docs.docker.com/config/containers/logging for more details on logging in Docker.

- :ref:`hge_logs` for more details on Hasura logs.

.. _docker_update:

Updating Hasura GraphQL engine
------------------------------

This guide will help you update the Hasura GraphQL engine running with Docker. This guide assumes that you already have
Hasura GraphQL engine running with Docker.

Step 1: Check the latest release version
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The current latest version is:

.. raw:: html

   <code>hasura/graphql-engine:<span class="latest-release-tag">latest</span></code>

All the versions can be found at: https://github.com/hasura/graphql-engine/releases

Step 2: Update the Docker image
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the ``docker-compose`` command that you're running, update the image tag to this
latest version.

For example, if you had:

.. raw:: html

   <code>
     graphql-engine:<br/>
     &nbsp;&nbsp;image: hasura/graphql-engine:v1.2.0
   </code>

you should change it to:

.. raw:: html

   <code>
     graphql-engine:<br/>
     &nbsp;&nbsp;image: hasura/graphql-engine:<span class="latest-release-tag">latest</span>
   </code>

.. note::

  If you are downgrading to an older version of the GraphQL engine you might need to downgrade your metadata catalogue version
  as described in :ref:`downgrade_hge`

Advanced
--------

- :ref:`Setting up migrations <migrations>`

