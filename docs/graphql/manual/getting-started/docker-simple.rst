.. meta::
   :description: Get started with Hasura using Docker
   :keywords: hasura, docs, start, docker

.. _docker_simple:

Quickstart with Docker
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide will help you get the Hasura GraphQL engine and Postgres running as
Docker containers using Docker Compose. This is the easiest way to set up
Hasura GraphQL engine on your **local environment**. 

In case you'd like to run Hasura on an existing Postgres database, follow this
guide to :ref:`deploy the Hasura GraphQL engine as a standalone docker container
<deployment_docker>` and connect it to your Postgres instance. 

Prerequisites
-------------

- `Docker <https://docs.docker.com/install/>`_
- `Docker Compose <https://docs.docker.com/compose/install/>`_

Step 1: Get the docker-compose file
----------------------------------- 

The `hasura/graphql-engine/install-manifests <https://github.com/hasura/graphql-engine/tree/stable/install-manifests>`_ repo
contains all installation manifests required to deploy Hasura anywhere. Get the docker compose file from there:

.. code-block:: bash

   # in a new directory run
   wget https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/docker-compose/docker-compose.yaml
   # or run
   curl https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/docker-compose/docker-compose.yaml  -o docker-compose.yml

Step 2: Run Hasura GraphQL engine & Postgres
--------------------------------------------

.. code-block::  bash

   $ docker-compose up -d

Check if the containers are running:

.. code-block:: bash

  $ docker ps

  CONTAINER ID IMAGE                 ... CREATED STATUS PORTS          ...
  097f58433a2b hasura/graphql-engine ... 1m ago  Up 1m  8080->8080/tcp ...
  b0b1aac0508d postgres              ... 1m ago  Up 1m  5432/tcp       ...

Step 3: Open the Hasura console
-------------------------------

Head to ``http://localhost:8080/console`` to open the Hasura console.

Hello World (GraphQL or event triggers)
---------------------------------------

Make your :ref:`first graphql query <first_graphql_query>`

OR

Set up your :ref:`first event trigger <first_event_trigger>`

Advanced
--------

This was a quickstart guide to get the Hasura GraphQL engine up and running
quickly. For more detailed instructions on deploying using Docker, check out
:ref:`deployment_docker`.

