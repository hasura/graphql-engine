Adding custom logic - using schema stitching & custom resolvers
===============================================================

Hasura GraphQL engine provides instant GraphQL APIs over the tables and views of any Postgres database by
auto-generating the CRUD resolvers. However, sometimes you might have other custom business logic that needs to be
run along with the simple CRUD operations.

Here are a couple of common use cases:

- Fetching data from sources that are not in the database (e.g. a weather API)
- Customizing mutations (e.g. running validations before inserts)

This can be achieved by adding custom resolvers and/or schema stitching.
**Custom resolvers** are custom code blocks that are executed when a particular field is requested for in a GraphQL
request while **schema stitching** is the process of merging multiple GraphQL schemas into one.

*TODO: add illustration*

The following are the different possible customizations possible to the Hasura GraphQL engine schema:

- Adding top-level fields to the Hasura GraphQL engine schema.
- Extending the GraphQL schema fields exposed by Hasura GraphQL engine.

Adding top-level fields to the Hasura GraphQL engine schema
-----------------------------------------------------------

This can be done using Hasura's :doc:`GraphQL schema stitching feature <schema-stitching>`.

Extending the fields exposed in the Hasura GraphQL engine schema
----------------------------------------------------------------

This can be done by writing :doc:`custom resolvers <custom-resolvers>` for the extra fields.

.. toctree::
  :maxdepth: 1

  schema-stitching
  custom-resolvers
