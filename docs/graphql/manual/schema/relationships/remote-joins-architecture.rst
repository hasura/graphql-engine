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

Introduction
------------

Hasura has a unique way of bringing data sources together and solving the data federation problem. This is done via remote joins wherein you can join data between data sources, similar to table relationships.

With remote joins, the join, authorization, and consistency checks (of added sources) all happen at the Hasura layer via metadata. This allows underlying data sources and APIs to evolve independently. Your applications have a unified API to query the full data landscape in your org.

Security
--------

Hasura's :ref:`authorization model <authorization>` gives you role-based access control. This extends to actions and remote schemas as well. In actions, you can go to the ``Permissions`` tab and choose which fields to expose for which roles. In remote schemas, Hasura will :ref:`forward <schema_auth>` your session variables which can be used to implement custom authorization in your remote schemas. Native role-based permissions on remote schemas is in the works.

Performance
-----------

Hasura strives for optimal performance. It creates an efficient execution plan which pushes down most of the heavy-lifting involved to underlying sources. For example, Hasura creates a single efficient query to your database and batches calls to remote schemas to avoid the `n+1 problem <https://hasura.io/learn/graphql/intro-graphql/graphql-server/>`__ . More improvements to performance are upcoming.

Schema integrity & consistency
------------------------------

Hasura has metadata consistency checks at every level. Whenever you add a table, remote schema, or action, Hasura makes sure that the graph that is exposed is consistent, and that all the relationships make sense at a type level. This helps you in creating robust workflows to test changes in your CI and making safe deployments of your unified graph.
