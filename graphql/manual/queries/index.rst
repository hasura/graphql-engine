Queries
=======

HasuraDB auto-generates queries and mutations as part of the GraphQL schema from your Postgres schema model. It generates a range of possible queries and operators that also work with relationships defined in your SQL schema. All tracked tables in the public schema of hasuradb database can be queried and modified over the GraphQL endpoint. If you have a table named “author” in your database, a query and a mutation each are added as nested fields under the root level types, query_root and mutation_root respectively. For e.g. the auto-generated field for the ‘author’ table may look like this:

.. code-block:: none

    author (
      where: author_bool_exp
      limit: Int
      offset: Int
      order_by: [String]
    ): [author]


You can explore the entire schema and the available queries using the GraphiQL interface in the API-Console or using the "Docs" link in the top, right corner of the embedded, interactive GraphiQL window in the docs. Let’s take a look at the different queries you can run using the HasuraDB GraphQL endpoint. We’ll use examples based on a typical Authors and Articles schema for reference.

.. toctree::
  :maxdepth: 1

  simple-object-queries
  nested-object-queries
  query-filters
  sorting
  pagination
  multiple-arguments
  multiple-queries
  control-access
