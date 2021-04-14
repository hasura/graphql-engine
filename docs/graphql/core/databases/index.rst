.. meta::
  :description: Hasura databases support
  :keywords: hasura, docs, databases

.. _databases:

Databases
=========

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The Hasura GraphQL engine automatically generates your GraphQL schema and resolvers based on tables/views
in your database . **You don't need to write a GraphQL schema or resolvers.** See
:ref:`How Hasura GraphQL engine works <how_it_works>` for more details.

The Hasura console gives you UI tools that speed up your data-modelling process, or working with your existing databases.
The console also automatically generates migrations or metadata files that you can edit directly and check into your
version control.

Supported databases
-------------------

.. toctree::
  :maxdepth: 1
  :titlesonly:

  postgres/index
  ms-sql-server/index
  Connecting databases <connect-db>
