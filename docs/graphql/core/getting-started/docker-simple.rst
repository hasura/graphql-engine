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

This guide will help you get the Hasura GraphQL engine and a Postgres database to store its metadata
running as Docker containers using Docker Compose. This is the easiest way to set up
Hasura GraphQL engine on your **local environment**.

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

Step 3: Connect a database
--------------------------

Head to ``http://localhost:8080/console`` to open the Hasura console.

Navigate to ``Data -> Manage -> Connect Database``:

.. thumbnail:: /img/graphql/cloud/getting-started/connect-db-console.png
  :alt: Connect database
  :width: 1000px

Enter your database connection URL.

Click ``Connect Database``.

.. thumbnail:: /img/graphql/core/getting-started/connect-db.png
  :alt: Enter URL for existing database
  :width: 700px

.. admonition:: Starting from scratch

  If you are looking to start setting up a backend from scratch, we recommend
  using Postgres as the database. If you do not have an existing Postgres database, you can choose
  to connect to the same Postgres database that is used to store the Hasura metadata.

  You can pick the database connection URL from the value assigned to the
  ``HASURA_GRAPHQL_METADATA_DATABASE_URL`` env var in the docker compose file and use
  that in this step.

Step 4: Try out Hasura
----------------------

.. TODO: add options to track existing vs create new tables

Create a table
^^^^^^^^^^^^^^

On the Hasura console, navigate to ``Data -> Create table`` and create a sample table called ``profiles`` with
the following columns:

.. code-block:: sql

  profiles (
    id SERIAL PRIMARY KEY, -- serial -> auto-incrementing integer
    name TEXT
  )

.. thumbnail:: /img/graphql/core/getting-started/create-profile-table.png
   :alt: Create a table

Now, insert some sample data into the table using the ``Insert Row`` tab of the ``profiles`` table.

Try out a query
^^^^^^^^^^^^^^^

Head to the ``GraphiQL`` tab in the console and try running the following query:

.. code-block:: graphql

    query {
      profiles {
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

Database operations
^^^^^^^^^^^^^^^^^^^

- :ref:`Database modelling <schema>`: Learn how to model your database schema, as well as how to extend it.
- :ref:`Querying data <queries>`: Use GraphQL queries to query data from your GraphQL API.
- :ref:`Inserting data <mutations>`: Use GraphQL mutations to insert data into your GraphQL API.

Business logic
^^^^^^^^^^^^^^

There are several options for the implementation of business logic, depending on your use case.

- :ref:`Actions <actions>`: Actions can be used if you'd like to extend your GraphQL schema by integrating with a REST endpoint.
- :ref:`Remote schemas <remote_schemas>`: If you have an existing GraphQL server or if you're comfortable with implementing one, you can use remote schemas.
- :ref:`Event triggers <event_triggers>`: To trigger a serverless function based on a database event, use event triggers.
- :ref:`Scheduled triggers <scheduled_triggers>`: Scheduled triggers are used to execute custom business logic at specific points in time.

Migrations
^^^^^^^^^^

Set up :ref:`Hasura migrations <migrations_setup>` to track your database alterations. This will make it easier to move to a different environment (e.g. staging or prod) later.

Secure your endpoint
^^^^^^^^^^^^^^^^^^^^

:ref:`Add an admin secret <docker_secure>` to make sure that your GraphQL endpoint and the Hasura console are not publicly accessible.

Detailed Docker setup
---------------------

This was a quickstart guide to get the Hasura GraphQL engine up and running
quickly. For more detailed instructions on deploying using Docker, check out
:ref:`deployment_docker`.
