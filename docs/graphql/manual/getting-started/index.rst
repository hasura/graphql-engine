.. meta::
   :description: Get started with Hasura
   :keywords: hasura, docs, start

.. _getting_started:

Getting started
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

To use the Hasura GraphQL engine, you need to:

#. Run the Hasura GraphQL engine with access to a Postgres database
#. Use the Hasura console (an admin UI) that connects to the Hasura GraphQL engine to help you build your schema and
   run GraphQL queries

.. thumbnail:: ../../../img/graphql/manual/getting-started/running-hasura.png
   :width: 75%
   :class: no-shadow
   :alt: Running Hasura

Get started from scratch
------------------------

- :ref:`Using Heroku <heroku_simple>` **(recommended)**: Get started in under 60 seconds with no setup required to
  host the Hasura GraphQL engine and Postgres on Heroku's free tier.
- :ref:`Using Docker <docker_simple>`: Run a **local development** setup that sets up both the Hasura GraphQL
  engine and Postgres using Docker.

Get started using an existing database
--------------------------------------

- :ref:`Using Heroku <heroku_existing_db>`: Run on Heroku using an existing Heroku
  Postgres database.
- :ref:`Using Docker <deployment_docker>`: Run as a docker container and connect to an existing Postgres
  database.
- :ref:`Using Kubernetes <deploy_kubernetes>`: Run on Kubernetes and connect to an existing Postgres
  database.

.. admonition:: Supported Postgres versions

  Hasura GraphQL engine supports **Postgres 9.5 and above**

.. toctree::
   :maxdepth: 2
   :titlesonly:
   :hidden:

   Heroku quickstart <heroku-simple>
   Docker quickstart <docker-simple>
   Using an existing database <using-existing-database>
   first-graphql-query
   first-event-trigger

