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

Introduction
------------

This guide will help you get the Hasura GraphQL engine and Postgres running as
Docker containers using Docker Compose. This is the easiest way to set up
Hasura GraphQL engine on your **local environment**. 

In case you'd like to run Hasura on an existing Postgres database, follow :ref:`this guide <deployment_docker>`
to deploy the Hasura GraphQL engine as a standalone docker container and connect it to your Postgres instance.

Prerequisites
-------------

- `Docker <https://docs.docker.com/install/>`__
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

Step 4: Try out Hasura
----------------------

Create a table
^^^^^^^^^^^^^^

On the Hasura console, navigate to ``Data -> Create table`` and create a sample table called ``profile`` with
the following columns:

.. code-block:: sql

  profile (
    id SERIAL PRIMARY KEY, -- serial -> auto-incrementing integer
    name TEXT
  )

.. thumbnail:: /img/graphql/core/getting-started/create-profile-table.png
   :alt: Create a table

Now, insert some sample data into the table using the ``Insert Row`` tab of the ``profile`` table.

Try out a query
^^^^^^^^^^^^^^^

Head to the ``GraphiQL`` tab in the console and try running the following query:

.. code-block:: graphql

    query {
      profile {
        id
        name
      }
    }

You'll see that you get all the inserted data!

.. thumbnail:: /img/graphql/core/getting-started/profile-query.png
   :alt: Try out a query

Next steps
----------

Learn course
^^^^^^^^^^^^

For a full hands-on tour of Hasura, check out our `30-Minute Hasura Basics Course <https://hasura.io/learn/graphql/hasura/introduction/>`__.

Database modelling
^^^^^^^^^^^^^^^^^^

Check out the :ref:`schema section <schema>` to learn about how to model your database schema, as well as how to extend it.

Querying data
^^^^^^^^^^^^^

Learn how to use :ref:`GraphQL queries <queries>` to query data from your GraphQL API.

Inserting data
^^^^^^^^^^^^^^

Learn how to insert data into your GraphQL API using :ref:`GraphQL mutations <mutations>`.

Business logic
^^^^^^^^^^^^^^

There are several options for the implementation of business logic, depending on your use case.

Actions
*******

:ref:`Actions <actions>` can be used if you'd like to extend your GraphQL schema by integrating with a REST endpoint.

Remote schema
*************

If you have an existing GraphQL server or if you're comfortable with implementing one, you can use :ref:`remote schemas <remote_schemas>`.

Event triggers
**************

To trigger a serverless function based on a database event, use :ref:`event triggers <event_triggers>`.

Scheduled triggers
******************

:ref:`Scheduled triggers <scheduled_triggers>` are used to execute custom business logic at specific points in time.

Migrations
^^^^^^^^^^

Set up :ref:`Hasura migrations <migrations_setup>` to track your database alterations. This will make it easier to move to a different environment (e.g. staging or prod) later.

Advanced Docker setup
---------------------

This was a quickstart guide to get the Hasura GraphQL engine up and running
quickly. For more detailed instructions on deploying using Docker with an
external database, check out :ref:`deployment_docker`.

- :ref:`Using Docker <deployment_docker>`: Run as a docker container and connect to an existing Postgres
  database.
