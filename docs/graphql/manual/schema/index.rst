Schema
======

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Hasura GraphQL engine automatically generates your GraphQL schema and resolvers based on your tables/views
in Postgres. **You don't need to write a GraphQL schema or resolvers.** See
:doc:`How Hasura GraphQL engine works <../how-it-works/index>` for more details.

The Hasura console gives you UI tools that speed up your data-modelling process, or working with your existing database.
The console also automatically generates migrations or metadata files that you can edit directly and check into your
version control.

Hasura GraphQL engine lets you do anything you would usually do with Postgres by giving you GraphQL over native
Postgres constructs.

**See:**

.. toctree::
  :maxdepth: 1

  Basics <basics>
  Relationships <relationships/index>
  Customise with views <views>
  Default field values <default-values/index>
  Enum type fields <enums>
  Using an existing database <using-existing-database>
  Setting up soft deletes <soft-deletes>
  Export GraphQL schema <export-graphql-schema>
