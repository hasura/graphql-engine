.. meta::
   :description: Get started with Hasura
   :keywords: hasura, docs, start

.. _getting_started:

Getting Started
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

To use the Hasura GraphQL engine, you need to:

- Deploy the Hasura GraphQL engine with access to a Postgres database
- Set up and test your GraphQL API using the Hasura console UI *(also possible via the CLI or API)*
- Consume the generated GraphQL API from your client apps

Get started from scratch
------------------------

- :ref:`Using Hasura Cloud <cloud_getting_started>` **(recommended)**: Create
  a new Hasura Cloud project with just a few clicks.
- :ref:`Using Docker <docker_simple>`: Run a **local development** setup that sets up both the Hasura GraphQL
  engine and Postgres using Docker Compose.

Get started using an existing database
--------------------------------------

- :ref:`Using Hasura Cloud <cloud_getting_started>` **(recommended)**: Create a new Hasura Cloud project connected to an existing Postgres database.
- :ref:`Using Docker <deployment_docker>`: Run as a docker container and connect to an existing Postgres
  database.
- :ref:`Using Kubernetes <deploy_kubernetes>`: Run on Kubernetes and connect to an existing Postgres
  database.

.. admonition:: Supported Postgres versions

  Hasura GraphQL engine supports **Postgres 9.5 and above**

.. admonition:: MS SQL Server support (alpha)

  Hasura GraphQL engine supports **SQL server 2016 and above**. Head to :ref:`this guide <database_ms-sql-server>`
  to get started with SQL Server.

.. admonition:: MySQL support (alpha)

  Head to :ref:`this guide <mysql_preview>` to try out the MySQL preview.

.. toctree::
   :maxdepth: 2
   :titlesonly:
   :hidden:

   Hasura Cloud quickstart <https://hasura.io/docs/1.0/graphql/cloud/getting-started/index.html>
   Docker quickstart <docker-simple>
   Using an existing database <using-existing-database>
   Making your first GraphQL query <first-graphql-query>

