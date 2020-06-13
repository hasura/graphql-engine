.. meta::
   :description: Remote schema joins with Hasura
   :keywords: hasura, docs, remote schema, remote joins, data federation

.. _remote_joins:

Remote joins  
------------

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Remote joins extend the concept of joining data across tables, to joining across tables *and* remote data sources. Once you create relationships between types from your database and types created from APIs, you can then "join" them by running GraphQL queries.

These APIs can be custom GraphQL servers you write, 3rd party SaaS APIs, or even other Hasura instances.

Because Hasura is meant to be a GraphQL server that you can expose directly to your apps, Hasura also handles security and authorization while providing remote joins.

.. note::
  For example use cases, check out this `blog post <https://hasura.io/blog/remote-joins-a-graphql-api-to-join-database-and-other-data-sources/>`_.

Creating a relationship
^^^^^^^^^^^^^^^^^^^^^^^

After you :ref:`add a remote schema <adding_schema>`, you can create remote relationships.

[TODO: steps & images]

Permissions
^^^^^^^^^^^
