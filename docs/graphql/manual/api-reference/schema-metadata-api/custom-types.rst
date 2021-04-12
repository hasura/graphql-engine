.. meta::
   :description: Define custom types with the Hasura schema/metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, custom types

.. _api_custom_types:

Schema/Metadata API Reference: Custom Types
===========================================

**Custom Types** are user-defined GraphQL types which help to define :ref:`Actions <api_actions>`.

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
     - Array of InputObjectType_
     - Set of GraphQL ``Input Object``
   * - objects
     - false
     - Array of ObjectType_
     - Set of GraphQL ``Object``
   * - scalars
     - false
     - Array of ScalarType_
     - Set of GraphQL ``Scalar``
   * - enums
     - false
     - Array of EnumType_
     - Set of GraphQL ``Enum``

.. _InputObjectType:

InputObjectType
&&&&&&&&&&&&&&&

A simple JSON object to define `GraphQL Input Object <https://spec.graphql.org/June2018/#sec-Input-Objects>`__

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`GraphQLName`
     - Name of the Input object type
   * - description
     - false
     - String
     - Description of the Input object type
   * - fields
     - true
     - Array of InputObjectField_
     - Fields of the Input object type

.. _InputObjectField:

InputObjectField
****************

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`GraphQLName`
     - Name of the Input object field
   * - description
     - false
     - String
     - Description of the Input object field
   * - type
     - true
     - :ref:`GraphQLType <GraphQLType>`
     - GraphQL ype of the input object field


.. _ObjectType:

ObjectType
&&&&&&&&&&

A simple JSON object to define `GraphQL Object <https://spec.graphql.org/June2018/#sec-Objects>`__

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`GraphQLName`
     - Name of the Object type
   * - description
     - false
     - String
     - Description of the Object type
   * - fields
     - true
     - Array of ObjectField_
     - Fields of the Object type
   * - relationships
     - false
     - Array of ObjectRelationship_
     - Relationships of the Object type to tables

.. _ObjectField:

ObjectField
***********

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`GraphQLName`
     - Name of the Input object field
   * - description
     - false
     - String
     - Description of the Input object field
   * - type
     - true
     - :ref:`GraphQLType <GraphQLType>`
     - GraphQL type of the input object field

.. _ObjectRelationship:

ObjectRelationship
******************

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`RelationshipName`
     - Name of the relationship, shouldn't conflict with existing field names
   * - type
     - true
     - [ ``object`` | ``array`` ]
     - Type of the relationship
   * - remote_table
     - true
     - :ref:`TableName`
     - The table to which relationship is defined
   * - field_mapping
     - true
     - Object (ObjectField_ name : Remote table's :ref:`PGColumn`)
     - Mapping of fields of object type to columns of remote table

.. _ScalarType:

ScalarType
&&&&&&&&&&

A simple JSON object to define `GraphQL Scalar <https://spec.graphql.org/June2018/#sec-Scalars>`__

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`GraphQLName`
     - Name of the Scalar type
   * - description
     - false
     - String
     - Description of the Scalar type

.. _EnumType:

EnumType
&&&&&&&&

A simple JSON object to define `GraphQL Enum <https://spec.graphql.org/June2018/#sec-Enums>`__

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`GraphQLName`
     - Name of the Enum type
   * - description
     - false
     - String
     - Description of the Enum type
   * - values
     - true
     - Array of EnumValue_
     - Values of the Enum type

.. _EnumValue:

EnumValue
*********

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - value
     - true
     - :ref:`GraphQLName`
     - Value of the Enum type
   * - description
     - false
     - String
     - Description of the value
   * - is_deprecated
     - false
     - Boolean
     - If set to ``true``, the enum value is marked as deprecated
