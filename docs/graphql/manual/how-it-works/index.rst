.. meta::
   :description: How Hasura GraphQL engine works
   :keywords: hasura, docs, graphql engine, how it works

.. _how_it_works:

How Hasura GraphQL engine works
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Given a Postgres database, the Hasura GraphQL engine can automatically generate a GraphQL schema and process GraphQL
queries, subscriptions and mutations. Here’s what the Hasura GraphQL engine does under the hood.

Schema generation
-----------------

The Hasura GraphQL engine automatically generates GraphQL schema components when you track a
Postgres table/view in Hasura and create relationships between them.

Tables
^^^^^^

When you track a Postgres table in the Hasura GraphQL engine, it automatically generates the following for it:

- A GraphQL type definition for the table
- A query field with ``where``, ``order_by``, ``limit`` and ``offset`` arguments
- A subscription field with ``where``, ``order_by``, ``limit`` and ``offset`` arguments
- An insert mutation field with ``on_conflict`` argument that supports upsert and bulk inserts
- An update mutation field with ``where`` argument that supports bulk updates
- A delete mutation field with ``where`` argument that supports bulk deletes

Views
^^^^^

When you track a Postgres view in Hasura GraphQL engine, it automatically generates the following for it:

- A GraphQL type definition for the view
- A query field with ``where``, ``order_by``, ``limit`` and ``offset`` arguments
- A subscription field with ``where``, ``order_by``, ``limit`` and ``offset`` arguments

Essentially the Hasura GraphQL engine does the same thing it would do for a table, but without creating the insert, update
and delete mutations.

Relationships
^^^^^^^^^^^^^
When you create a relationship between a table/view with another table/view in the Hasura GraphQL engine, it does the
following:

- Augments the type of the table/view by adding a reference to the nested type to allow fetching nested objects.
- Augments the ``where`` and ``order_by`` clauses to allow filtering and sorting based on nested objects.

Resolvers
---------

The Hasura GraphQL engine does not have any resolvers. The Hasura GraphQL engine is actually a compiler that compiles
your GraphQL query into an efficient SQL query.

Hasura's GraphQL syntax is also optimized to expose the power of the underlying SQL so that you can make powerful
queries via GraphQL.

Metadata
--------

All the information required for schema generation is stored by the Hasura GraphQL engine as metadata in its "catalogue"
which is essentially a special Postgres schema in the database.

See :ref:`metadata catalogue <hasura_metadata_schema>` for more details.

.. toctree::
  :maxdepth: 1
  :hidden:

  Metadata catalogue <metadata-schema>
