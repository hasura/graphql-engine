.. meta::
   :description: Security, performance, schema integrity, and data federation with Hasura remote joins
   :keywords: hasura, docs, data federation, remote relationship, remote join, remote schema

.. _remote_joins_architecture:

Architecture of remote joins
============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Security
^^^^^^^^

Hasura's :ref:`authorization model <authorization>` gives you role-based access control. This extends to actions and remote schemas. In actions, you can go to the "Permissions" tab and choose which fields to expose for which roles. In remote schemas, Hasura will forward your session variables to your custom authorization implementation. More fine-grained role-based exposure of remote schema fields is in the works.

Performance
^^^^^^^^^^^

Hasura strives for optimal performance. It creates an execution plan, which pushes down most of the heavy-lifting involved in a query, sending a single efficient query to your database or your remote GraphQL endpoints, avoiding the `n+1 problem <https://hasura.io/learn/graphql/intro-graphql/graphql-server/>`__ in the process.

Schema integrity & consistency
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Hasura has metadata consistency checks at every level. Whenever you add a table, remote schema, or action, Hasura makes sure that the graph that is exposed is consistent, and that all the relationships make sense at a type level. This helps with setting up tooling, such as integration tests in your CI for safe changes.

Data federation
^^^^^^^^^^^^^^^

Hasura has a unique solution to the data federation problem, in that the join, authorization, and consistency checks all happen at the Hasura layer, allowing underlying data sources and APIs to evolve independently. With this model, your applications have a unified API with which to harness the full power of your data.
