.. meta::
  :description: Hasura BigQuery database support
  :keywords: hasura, docs, databases, bigquery

.. _database_bigquery:

BigQuery
========

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Hasura allows connecting to a `BigQuery <https://cloud.google.com/bigquery>`__ database and building a GraphQL API based on the database schema.

.. admonition:: Supported versions:

  Hasura GraphQL engine ``v2.0.0-alpha.1`` onwards

Get Started
-----------

To try Hasura with BigQuery, you'll need your own new or existing BigQuery database.

Here is how you can get started with Hasura:

:ref:`Getting Started with BigQuery <database_bigquery_getting_started>`

.. TODO: DB COMPATIBILITY

  1. :ref:`Hasura Cloud<database_bigquery_cloud>`: You'll need to be able to access your SQL Server database from Hasura Cloud.
  2. :ref:`Docker<database_bigquery_docker>`: Run Hasura with Docker and then connect your SQL Server database to Hasura.

Keep up to date
---------------

Hasura currently supports queries on BigQuery.

Please watch this space to get the latest docs on how you can try these features out via the console or by manipulating metadata in JSON/YAML directly.

If you'd like to stay informed about the status of BigQuery support, subscribe to our newsletter and join our discord!

- https://hasura.io/newsletter/
- https://discord.com/invite/hasura

Know more
---------

.. toctree::
  :maxdepth: 1
  :titlesonly:

  Getting Started <getting-started>
  Schema <schema/index>

.. TODO: DB-COMPATIBILITY

  .. toctree::
    :maxdepth: 1
    :titlesonly:

    Schema <schema/index>
    Queries <queries/index>
    Mutations <mutations/index>
    Subscriptions <subscriptions/index>
    Supported MS SQL Server types <mssql-types>
