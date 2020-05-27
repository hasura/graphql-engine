.. meta::
   :description: Manage remote relationships with the Hasura schema/metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, remote joins, remote relationships

Schema/Metadata API Reference: Remote Relationships
===================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Remote Relationships allow you to join tables with remote schemas.

.. _create_remote_relationship:

create_remote_relationship
--------------------------

``create_remote_relationship`` is used to create a new remote relationship with an existing remote schema.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "type":"create_remote_relationship",
      "args":{
         "name":"sample_remote_relationship",
         "table":"users",
         "hasura_fields":[
           "id"
         ],
         "remote_schema":"my-remote-schema",
         "remote_field":{
           "messages":{
              "arguments":{
                 "id":"$id"
              }
           }
      }
   }

.. _create_remote_relationship_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - RemoteRelationshipName_
     - Name of the remote relationship
   * - table
     - true
     - :ref:`QualifiedTable <QualifiedTable>`
     - Object with table name and schema
   * - hasura_fields
     - true
     - [:ref:`PGColumn <PGColumn>`]
     - Column(s) in the table that is used for joining with remote schema field. All join keys in ``remote_field`` must appear here.
   * - remote_schema
     - true
     - :ref:`RemoteSchemaName <RemoteSchemaName>`
     - Name of the remote schema to join with
   * - remote_field
     - true
     - RemoteField_
     - Name of the field in remote schema with the join arguments.

.. _update_remote_relationship:

update_remote_relationship
--------------------------

``update_remote_relationship`` is used to update an existing remote relationship.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
     "type": "update_remote_relationship",
     "args": {
        "name": "sample_remote_relationship",
        "table": "users",
        "hasura_fields": ["id"],
        "remote_schema": "my-remote-schema",
        "remote_field": {
          "posts": {
             "arguments": {
                "id":"$id",
                "where":{
                   "likes":{
                   "lte":"1000"
                },
                "limit":100
          }
        }
     }
   }

.. _update_remote_relationship_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - RemoteRelationshipName_
     - Name of the remote relationship
   * - table
     - true
     - :ref:`QualifiedTable <QualifiedTable>`
     - Object with table name and schema
   * - hasura_fields
     - true
     - [:ref:`PGColumn <PGColumn>`]
     - Column(s) in the table that is used for joining with remote schema field. All join keys in ``remote_field`` must appear here.
   * - remote_schema
     - true
     - :ref:`RemoteSchemaName <RemoteSchemaName>`
     - Name of the remote schema to join with
   * - remote_field
     - true
     - RemoteField_
     - Name of the field in remote schema with the join arguments.

.. _delete_remote_relationship:

delete_remote_relationship
--------------------------

``delete_remote_relationship`` is used to delete an existing remote relationship.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "delete_remote_relationship",
       "args" : {
          "table":{
             "name":"users",
             "schema":"public"
          },
          "name":"sample_remote_relationship"
       }
   }

.. _delete_remote_relationship_syntax:

``delete_remote_relationship`` will delete an existing remote relationship.

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`QualifiedTable <QualifiedTable>`
     - Object with table name and schema
   * - name
     - true
     - RemoteRelationshipName_
     - Name of the remote relationship

.. _RemoteRelationshipName:

RemoteRelationshipName
&&&&&&&&&&&&&&&&&&&&&&

.. parsed-literal::

  String


RemoteField
&&&&&&&&&&&

Examples:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "message": {
         "arguments":{
            "message_id":"$id"
          }
      }
   }

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "messages": {
         "arguments": {
            "where": {
               "id": {
                  "gt": "$id"
               },
            "limit": 100
         }
      }
   }

The `RemoteField` expects the following schema:

.. code-block:: json

  FieldName: {
    "arguments": InputArguments
  }


The ``FieldName`` is the top-level node exposed by the remote schema.

The `arguments` field is used to specify how to join the specified Hasura table to the remote schema table.
