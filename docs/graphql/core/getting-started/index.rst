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

- Deploy the Hasura GraphQL engine, with access to a Postgres database to store its metadata.
- Connect new/existing database(s) and set up and test your GraphQL API using the Hasura console UI *(also possible via CLI or API)*.
- Consume the generated GraphQL API from your client apps.

Quickstart guides
-----------------

- :ref:`Using Hasura Cloud <cloud_getting_started>` - Create a new Hasura Cloud project with just a few clicks.

  - Recommended as it's **the fastest way** to try Hasura GraphQL engine out.
  - Comes with **extra features for reliability and security** and a managed metadata database.

Hasura is open-source and can also be downloaded locally. :ref:`Set up Hasura GraphQL engine using Docker Compose <docker_simple>`. This is recommended if you want to **run Hasura locally** or if your database cannot be exposed to Hasura Cloud.

.. TODO: add link to detailed deployment guides post 2.0 content update

Supported databases
-------------------

Hasura GraphQL engine supports:

- :ref:`Postgres <database_postgres>`
- :ref:`MS SQL Server <database_ms_sql_server>`
- :ref:`Citus - Hyperscale <database_citus-hyperscale-postgres>`
- :ref:`BigQuery <database_bigquery>`
- **MySQL** (in preview) - See the :ref:`preview guide <mysql_preview>`

.. admonition:: Additional Resources

  Get Started with Hasura today - `Watch video guide <https://hasura.io/events/webinar/get-started-with-hasura/?pg=docs&plcmt=body&cta=getting-started&tech=>`__.

.. toctree::
   :maxdepth: 2
   :titlesonly:
   :hidden:

   Quickstart with Hasura Cloud <https://hasura.io/docs/latest/graphql/cloud/getting-started/index.html>
   Quickstart with Hasura using Docker <docker-simple>
   Making your first GraphQL query <first-graphql-query>

