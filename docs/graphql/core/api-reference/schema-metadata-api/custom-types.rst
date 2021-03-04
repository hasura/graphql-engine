.. meta::
   :description: Define custom types with the Hasura schema/metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, custom types

.. _api_custom_types:

Schema/Metadata API Reference: Custom Types
===========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

**Custom Types** are user-defined GraphQL types which help to define :ref:`Actions <api_actions>`.

.. admonition:: Deprecation

  In versions ``v2.0.0`` and above, the schema/metadata API is deprecated in favour of the :ref:`schema API <schema_apis>` and the
  :ref:`metadata API <metadata_apis>`.

  Though for backwards compatibility, the schema/metadata APIs will continue to function.

.. _set_custom_types:

set_custom_types
----------------

``set_custom_types`` is used to set user-defined GraphQL types. This API will replace the given types with existing ones.


.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
     "type": "set_custom_types",
     "args": {
       "scalars": [],
       "enums": [],
       "input_objects": [
         {
           "name": "User",
           "fields": [
             {
               "name": "username",
               "type": "String!"
             },
             {
               "name": "password",
               "type": "String!"
             }
           ]
         }
       ],
       "objects": [
         {
           "name": "UserId",
           "fields": [
             {
               "name": "id",
               "type": "Int!"
             }
           ],
           "relationships": [
             {
               "name": "posts",
               "type": "array",
               "remote_table": "post",
               "field_mapping": {
                 "id": "user_id"
               }
             }
           ]
         }
       ]
     }
   }


.. _set_custom_types_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - input_objects
     - false
     - Array of :ref:`InputObjectType`
     - Set of GraphQL ``Input Object``
   * - objects
     - false
     - Array of :ref:`ObjectType`
     - Set of GraphQL ``Object``
   * - scalars
     - false
     - Array of :ref:`ScalarType`
     - Set of GraphQL ``Scalar``
   * - enums
     - false
     - Array of :ref:`EnumType`
     - Set of GraphQL ``Enum``