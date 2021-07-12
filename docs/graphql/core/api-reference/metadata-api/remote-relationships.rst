.. meta::
   :description: Manage remote relationships with the Hasura metadata API
   :keywords: hasura, docs, metadata API, API reference, remote joins, remote relationships

Metadata API Reference: Remote Relationships (v2.0 and above)
=============================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Remote Relationships allow you to join tables with remote schemas.

.. admonition:: Supported from

  The metadata API is supported for versions ``v2.0.0`` and above and replaces the older
  :ref:`schema/metadata API <schema_metadata_apis>`.

.. _metadata_create_remote_relationship:

create_remote_relationship
--------------------------

``create_remote_relationship`` is used to create a new remote relationship with an existing remote schema.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "type":"create_remote_relationship",
      "args":{
         "name": "sample_remote_relationship",
         "table": "users",
         "hasura_fields": ["id"],
         "remote_schema": "my-remote-schema",
         "remote_field": {
           "messages": {
              "arguments": {
                 "id":"$id"
              }
           }
         }
      }
   }

.. _metadata_create_remote_relationship_syntax:

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
     - :ref:`RemoteRelationshipName`
     - Name of the remote relationship
   * - table
     - true
     - :ref:`QualifiedTable <QualifiedTable>`
     - Object with table name and schema
   * - hasura_fields
     - true
     - [:ref:`PGColumn <PGColumn>` | :ref:`ComputedFieldName <ComputedFieldName>`]
     - Column/Computed field(s) in the table that is used for joining with remote schema field. All join keys in ``remote_field`` must appear here.
   * - remote_schema
     - true
     - :ref:`RemoteSchemaName <RemoteSchemaName>`
     - Name of the remote schema to join with
   * - remote_field
     - true
     - :ref:`RemoteField`
     - The schema tree ending at the field in remote schema which needs to be joined with.

.. _metadata_update_remote_relationship:

update_remote_relationship
--------------------------

``update_remote_relationship`` is used to update an existing remote relationship.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
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
                "id": "$id",
                "likes": {
                   "lte":"1000"
                }
             }
          }
        }
     }
   }

.. _metadata_update_remote_relationship_syntax:

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
     - :ref:`RemoteRelationshipName`
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
     - :ref:`RemoteField`
     - The schema tree ending at the field in remote schema which needs to be joined with.

.. _metadata_delete_remote_relationship:

delete_remote_relationship
--------------------------

``delete_remote_relationship`` is used to delete an existing remote relationship.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
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

.. _metadata_delete_remote_relationship_syntax:

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
     - :ref:`RemoteRelationshipName`
     - Name of the remote relationship
