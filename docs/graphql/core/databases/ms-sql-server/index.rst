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

Hasura allows connecting to a SQL Server database and build a GraphQL API based on the database schema.

.. admonition:: Supported versions:

  1. Hasura GraphQL engine ``v2.0.0-alpha.2`` onwards
  2. SQL Server 2016 and upwards

Get Started
-----------

To try Hasura with SQL Server, you'll need your own new or existing SQL Server database.

Here are 2 ways you can get started with Hasura and SQL Server:

1. :ref:`Hasura Cloud <database_ms_sql_server_cloud>`: You'll need to be able to access your SQL Server database from Hasura Cloud.
2. :ref:`Docker <database_ms_sql_server_docker>`: Run Hasura with Docker and then connect your SQL Server database to Hasura.

Supported features
------------------

Hasura currently supports queries, subscriptions, inserts, deletes, relationships and permissions on MS SQL Server.

Next up on our roadmap for Hasura + SQL Server:

- Support for stored procedures & functions (`#7073 <https://github.com/hasura/graphql-engine/issues/7073>`__)
- Mutations: Run updates, stored procedures and transactions securely on SQL Server over a GraphQL API (`#7074 <https://github.com/hasura/graphql-engine/issues/7074>`__)
- Event triggers: Trigger HTTP webhooks with atomic capture and atleast once guarantee whenever data changes inside the database (`#7075 <https://github.com/hasura/graphql-engine/issues/7075>`__)
- Remote Joins: Join models in SQL Server to models from other API services (GraphQL or REST) (`#7076 <https://github.com/hasura/graphql-engine/issues/7076>`__)

Keep up to date
---------------

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
  Mutations <mutations/index>
  Subscriptions <subscriptions/index>

.. TODO: DB-COMPATIBILITY

  .. toctree::
    :maxdepth: 1
    :titlesonly:

    Supported MS SQL Server types <mssql-types>
