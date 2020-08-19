.. meta::
   :description: Manage remote relationships in Hasura
   :keywords: hasura, docs, schema, relationship, remote relationship

.. _remote_relationships:

Remote relationships
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Remote relationships (aka "remote joins") allow you to join data across tables and remote data sources. The remote data source can either be a :ref:`remote schema <remote_schemas>`, or the type returned from an :ref:`action <actions>`. Once you create relationships between types from your database and types created from APIs or actions, you can then "join" them by running GraphQL queries.

See the following guides on how to create different types of remote relationships:

- :ref:`remote_schema_relationships`: To join data across tables and remote APIs, such as custom GraphQL servers you write, third party SaaS APIs, or even other Hasura instances. For example, you can join customer data from your tables with account data from Stripe, Spotify, or Auth0.
- :ref:`action_relationships`: To join data across tables and actions. For example, you can join user data from your database with the response from a ``createUser`` action, using the ``id`` field.

Benefits
--------

Hasura's remote joins architecture provides the following benefits.

- **Security**: Hasura's :ref:`authorization model <authorization>` gives you role-based access control. This extends to actions and remote schemas as well. In actions, you can go to the ``Permissions`` tab and choose which fields to expose for which roles. For remote schemas, Hasura will :ref:`forward your session variables <schema_auth>` which can be used to implement custom authorization in your remote schemas. Native role-based permissions on remote schemas is in the works.

- **Performance**: Hasura strives for optimal performance. It creates an efficient execution plan which pushes down most of the heavy-lifting involved to underlying sources. For example, Hasura creates a single efficient query to your database and batches calls to remote schemas to avoid the `n+1 problem <https://hasura.io/learn/graphql/intro-graphql/graphql-server/>`__ . More improvements to performance are upcoming.

- **Schema integrity & consistency**: Hasura has metadata consistency checks at every level. Whenever you add a table, remote schema, or action, Hasura makes sure that the graph that is exposed is consistent, and that all the relationships make sense at a type level. This helps you in creating robust workflows to test changes in your CI and making safe deployments of your unified graph.

- **Data federation**: With remote joins, the join, authorization, and consistency checks of added sources all happen at the Hasura layer via metadata. This allows underlying data sources and APIs to evolve independently. Your applications have a unified API to query the full data landscape in your org.

.. toctree::
  :maxdepth: 1
  :hidden:

  remote-schema-relationships
  action-relationships
