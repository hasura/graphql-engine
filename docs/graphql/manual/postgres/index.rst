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

This page introduces Postgres including benefits and caveats, and it links to sections relevant to extending your GraphQL API by using Postgres features.

About Postgres
--------------

Postgres (or PostgreSQL) is a general-purpose object-relational database management system that uses and extends the SQL language.
Postgres is free and open source, and it's considered the most advanced open source database systems.

Postgres supports advanced data types and advanced performance optimization, features that are otherwise only available in expensive commercial database systems.

Benefits of Postgres
--------------------

The following are benefits of Postgres compared to other database management systems:

- Postgres can be maintained easily because of its stability. The total cost of ownership is therefore low.
- Postgres is designed to be extensible i.e. you can add custom functions using different programming languages.
- You can define your own data types, index types etc.
- If you need any support, an active community is available to help. 

Caveats
-------

Not all open source applications support Postgres as MySQL is generally a bit more widely spread. Also, when it comes to performance, MySQL has a slight edge.

.. note::

  MySQL support for Hasura is in the works.

Postgres features
-----------------

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
