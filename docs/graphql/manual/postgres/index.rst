.. meta::
   :description: Manage remote schemas with Hasura
   :keywords: hasura, docs, remote schema

.. _postgres:

Postgres Guide
==============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This page is a Postgres reference demonstrating the most common Postgres features that you can use to enhance your GraphQL API. 

About Postgres
--------------

PostgreSQL is a powerful, open source object-relational database system that uses and extends the SQL language combined with many features that safely store and scale the most complicated data workloads. 
In addition to being free and open source, PostgreSQL is highly extensible.

PostgreSQL is a general purpose and object-relational database management system, the most advanced open source database system. 
PostgreSQL is free and open source software. Its source code is available under PostgreSQL license, a liberal open source license.
PostgreSQL requires very minimum maintained efforts because of its stability.  Therefore, if you develop applications based on PostgreSQL, the total cost of ownership is low in comparison with other database management systems.
Used by big and small companies.

PostgreSQL supports advanced data types and advance performance optimization, features only available in the expensive commercial database, like Oracle and SQL Server.

Benefits of Postgres
--------------------

PostgreSQL is a general-purpose object-relational database management system. It allows you to add custom functions developed using different programming languages such as C/C++, Java, etc.
PostgreSQL is designed to be extensible.
In PostgreSQL, you can define your own data types, index types, functional languages, etc
If you need any support, an active community is available to help. 

.. admonition:: Caveats

  Postgres is not owned by one organization. 

Postgres features & Hasura
--------------------------

Postgres has some features that can be used to extend your Hasura GraphQL API. 
They are described in the following sections:

- :ref:`Constraints <postgres_constraints>`
- :ref:`Views <postgres_views>`
- :ref:`Functions <postgres_functions>`
- :ref:`Triggers <postgres_triggers>`
- :ref:`Indexes <postgres_indexes>`

.. toctree::
  :maxdepth: 1
  :hidden:

  Constraints <constraints>
  Views <views>
  Functions <functions>
  Triggers <triggers>
  Indexes <indexes>
