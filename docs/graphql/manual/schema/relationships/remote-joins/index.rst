.. meta::
   :description: Remote schema joins with Hasura
   :keywords: hasura, docs, remote schema, remote join, data federation

.. _remote_joins:

Remote joins & relationships
============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Remote joins extend the concept of joining data across tables, to joining across tables *and* remote data sources. Once you create relationships between types from your database and types created from APIs, you can then "join" them by running GraphQL queries.

These APIs can be custom GraphQL servers you write, 3rd party SaaS APIs, or even other Hasura instances.

Because Hasura is meant to be a GraphQL server that you can expose directly to your apps, Hasura also handles security and authorization while providing remote joins.

.. note::
  For more examples, check out this `blog post <https://hasura.io/blog/remote-joins-a-graphql-api-to-join-database-and-other-data-sources/>`__.

**See:**

.. toctree::
   :maxdepth: 1
 
   add



TODO:
-----


--

Join REST API with table (Action relationship)

1. Create Action
[img]
2. Go to the relationship tab of the Action
[img]
3. Join id of the action's returned type to the id of the table
[img]
4. GraphiQL
[img of mutation with joined fields in return]
- if table is joined with a remote schema, you can get those fields too
[img of fields from remote schema]

--

Check out voyager to see your entire API graph

--

Permissions
^^^^^^^^^^^

--

Performance
^^^^^^^^^^^

1. Push down the heavy lifting as much as possible.
2. Compile SQL -> One query. Avoids n+1.
3. Single HTTP call for remote joins. Avoids n+1.


Schema integrity & consistency
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
1. Hasura has metadata consistency checks at every level. Whenever you add a table, remote schema, or action, it makes sure that the graph that is exposed is consistent, that all the relationships make sense at a type level, etc. This helps you in setting up tooling--
2. Can run integration test in CI for safe changes. E.g. error if you removed a field in remote schema that was used in a join.

Data federation
^^^^^^^^^^^^^^^

- The join, authorization, and consistency checks happen at the Hasura layer. Underlying data sources and APIs can evolve independently.
- Your applications have a unified API with which to harness the full power of your data.

[img from Tiru's demo]
