.. meta::
  :description: Hasura MS SQL Server database support
  :keywords: hasura, docs, databases, ms sql, sql server

.. _database_ms_sql_server:

MS SQL Server
=============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Hasura allows connecting to a SQL Server database and build an GraphQL API based on the database schema.

.. admonition:: Supported versions:

  1. Hasura GraphQL engine ``v2.0.0-alpha.2`` onwards
  2. SQL Server 2016 and upwards

Get Started
-----------

To try Hasura with SQL Server, you'll need your own new or existing SQL Server database.

Here are 2 ways you can get started with Hasura:

1. :ref:`Hasura Cloud<database_ms_sql_server_cloud>`: You'll need to be able to access your SQL Server database from Hasura Cloud.
2. :ref:`Docker<database_ms_sql_server_docker>`: Run Hasura with Docker and then connect your SQL Server database to Hasura.

Keep up to date
---------------

Hasura supports queries, subscriptions, relationships and permissions on MS SQL Server.

Please watch this space to get the latest docs on how you can try these features out via the console or by manipulating metadata in JSON/YAML directly.

If you'd like to stay informed about the status of SQL Server support, subscribe to our newsletter and join our discord!

- https://hasura.io/newsletter/
- https://discord.com/invite/hasura

.. admonition:: Additional Resources

  This Hands-on Demo walks you through Getting Started with Hasura on SQL Server & common use cases. - `View Recording here <https://hasura.io/events/webinar/hasura-sql-server/?pg=docs&plcmt=body&cta=view-recording&tech=>`__.

Know more
---------

.. toctree::
  :maxdepth: 1
  :titlesonly:

  Getting Started <getting-started/index>
  Schema <schema/index>
  Queries <queries/index>
  Subscriptions <subscriptions/index>

.. TODO: DB-COMPATIBILITY

  .. toctree::
    :maxdepth: 1
    :titlesonly:

    Schema <schema/index>
    Queries <queries/index>
    Mutations <mutations/index>
    Subscriptions <subscriptions/index>
    Supported MS SQL Server types <mssql-types>
