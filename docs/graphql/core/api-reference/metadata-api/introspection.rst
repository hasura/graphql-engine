.. meta::
   :description: Manage introspection with the Hasura metadata API
   :keywords: hasura, docs, metadata API, API reference, introspection options, disable introspection

.. _metadata_graphql_introspection_:

========================================================================
 Metadata API Reference: GraphQL Introspection Options (v2.0 and above)
========================================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
============

API to set GraphQL introspection options. One of the options is to disable
introspection for the specified roles.

.. _metadata_set_graphql_introspection_options:

set_graphql_introspection_options
=================================

``set_graphql_schema_introspection_options`` is used to set graphql introspection options. Calling this API will
replace existing (if any) introspection options.

This API can be used to disable graphql introspection for the specified roles.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "set_graphql_schema_introspection_options",
       "args": {
           "disabled_for_roles": [
               "guest",
               "public"
           ]
       }
   }

.. _set_graphql_schema_introspection_options_syntax:

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - disabled_for_roles
     - true
     - Array of :ref:`RoleName`
     - Roles for which GraphQL schema introspection should be disabled *(supported only in cloud/enterprise versions)*
