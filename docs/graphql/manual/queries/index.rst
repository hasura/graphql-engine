Queries
=======

Hasura GraphQL engine auto-generates queries and mutations as part of the GraphQL schema from your Postgres schema
model. It generates a range of possible queries and operators that also work with relationships defined in your SQL
schema.

All tracked tables of the database can be queried and modified over the GraphQL endpoint. If you have a tracked table
in your database, a query and insert/update/delete mutations each are added as nested fields under the root level
types, ``query_root`` and ``mutation_root`` respectively. For example, the auto-generated query schema for an
"author" table may look like this:

.. code-block:: graphql

    author (
      where: author_bool_exp
      limit: Int
      offset: Int
      order_by:  [author_order_by!]
    ): [author]

**Note:** If a table is in any Postgres schema other than the ``public`` schema, the query field will be of the format
``<schema_name>_<table_name>``.

You can explore the entire schema and the available queries using the GraphiQL interface in the Hasura console.

Let’s take a look at the different queries you can run using the Hasura GraphQL engine. We’ll use examples
based on a typical author/article schema for reference.

.. toctree::
  :maxdepth: 1

  simple-object-queries
  nested-object-queries
  query-filters
  sorting
  pagination
  Using multiple arguments <multiple-arguments>
  multiple-queries
  aggregations
  control-access
