.. meta::
   :description: Hasura Cloud disable GraphQL introspection
   :keywords: hasura, docs, cloud, security, introspection, disable, GraphQL

.. _disable_graphql_introspection:

Disable GraphQL introspection
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The GraphQL engine by default enables `GraphQL Schema Introspection <http://spec.graphql.org/June2018/#sec-Schema-Introspection>`__
which gives full information about the schema and may not be desired in production.

To avoid this, you can disable GraphQL introspection on a per-role basis.

Disabling GraphQL introspection for a role
------------------------------------------

Introspection can be disabled for a role as shown below:

.. thumbnail:: /img/graphql/cloud/security/disable-introspection.png
   :alt: Hasura Cloud Console allow list tab
