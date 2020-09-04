.. meta::
   :description: Manage queries with Hasura
   :keywords: hasura, docs, query

.. _queries:

Queries
=======

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:
  

GraphQL queries are used to fetch data from the server.

Hasura GraphQL engine auto-generates queries as part of the GraphQL schema from your Postgres schema model.
It generates a range of possible queries and operators that also work with relationships defined in your SQL
schema.

All tables of the database tracked by the GraphQL engine can be queried over the GraphQL endpoint.
If you have a tracked table in your database, its query field is added as a nested
field under the ``query_root`` root level type.

.. toctree::
  :maxdepth: 1

  Postgres <postgres/index>
  MySQL <mysql/index>
