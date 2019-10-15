How Hasura GraphQL engine works
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Given a Postgres database, the Hasura GraphQL Engine can automatically generate a GraphQL Schema and process GraphQL
Queries, Subscriptions, and Mutations. Here’s what the Hasura GraphQL Engine does under the hood.

Schema generation
-----------------

The Hasura GraphQL Engine automatically generates GraphQL Schema components when you track a
Postgres Table/view in Hasura and create Relationships between them.

Tables
^^^^^^

When you track a Postgres table in the Hasura GraphQL engine, it automatically generates the following for it:

- A GraphQL type definition for the table
- A query field with ``Where``, ``Order_by``, ``Limit`` and ``Offset`` arguments
- A subscription field with ``Where``, ``Order_by``, ``Limit`` and ``Offset`` arguments
- An insert mutation field with ``On_conflict`` argument that supports Upsert and Bulk Inserts
- An update mutation field with ``where`` argument that supports bulk updates
- A delete mutation field with ``Where`` argument that supports Bulk Deletes

Views
^^^^^

When you track a Postgres view in Hasura GraphQL Engine, it automatically generates the following for it:

- A GraphQL type definition for the View
- A query field with ``Where``, ``Order_by``, ``Limit`` and ``Offset`` arguments
- A subscription field with ``Where``, ``Order_by``, ``Limit`` and ``Offset`` arguments

Essentially the Hasura GraphQL engine does the same thing it would do for a table, but without creating the insert, update
and delete mutations.

Relationships
^^^^^^^^^^^^^
When you create a Relationship between a table/view with another table/view in the Hasura GraphQL engine, it does the
following:

- Augments the type of the table/view by adding a reference to the nested type to allow fetching Nested Objects.
- Augments the ``where`` and ``order_by`` clauses to allow filtering and sorting based on Nested Objects.

Resolvers
---------

The Hasura GraphQL engine does not have any resolvers. The Hasura GraphQL engine is actually a compiler that compiles
your GraphQL query into an efficient SQL query.

Hasura's GraphQL syntax is also optimized to expose the power of the underlying SQL so that you can make powerful
Queries via GraphQL.

Metadata
--------

All the information required for schema generation is stored by the Hasura GraphQL Engine as Metadata in a specific
Postgres schema in the database. See :doc:`metadata schema <metadata-schema>` for more details.

.. toctree::
  :maxdepth: 1
  :hidden:

  Metadata schema <metadata-schema>
