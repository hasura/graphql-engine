.. meta::
   :description: Useful Postgres concepts for Hasura
   :keywords: hasura, docs, postgres

.. _postgres_basics:

Postgres basics
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This page introduces Postgres including benefits and caveats, and it links to sections relevant to extending your GraphQL API by using Postgres features.

About Postgres
--------------

`Postgres (or PostgreSQL) <https://www.postgresql.org/>`__ is a general-purpose object-relational database management system that uses and extends the SQL language.
Postgres is free and open source and has been developed continuously over the last 30 years. Postgres strives to be reliable, robust and performant.

Postgres supports advanced data types and advanced performance optimization, features that are otherwise only available in expensive commercial database systems.

Benefits of Postgres
--------------------

The following are benefits of Postgres compared to other database management systems:

- Postgres can be maintained easily because of its stability. The total cost of ownership is therefore low.
- Postgres is designed to be extensible i.e. you can add custom functions using different programming languages.
- You can define your own data types, index types etc.
- If you need any support, an `active community <https://www.postgresql.org/community/>`__ is available to help. 

Postgres features
-----------------

The following are some Postgres features that can be used to manage your data and extend your Hasura GraphQL API:

- :ref:`Constraints <postgres_constraints>`
- :ref:`Views <postgres_views>`
- :ref:`Functions <postgres_functions>`
- :ref:`Triggers <postgres_triggers>`
- :ref:`Indexes <postgres_indexes>`
- :ref:`Import data from CSV <postgres_import_data_from_csv>`

.. toctree::
  :maxdepth: 1
  :hidden:

  Constraints <constraints>
  Views <views>
  Functions <functions>
  Triggers <triggers>
  Indexes <indexes>
  Import data from CSV <import-data-from-csv>
