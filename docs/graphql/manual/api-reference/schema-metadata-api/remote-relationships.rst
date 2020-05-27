.. meta::
   :description: Manage remote relationships with the Hasura schema/metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, remote joins, remote relationships

Schema/Metadata API Reference: Remote Relationships
===================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Remote Joins will ensure independent and related types in Remote Schemas can be joined natively via Hasura.

.. _create_remote_relationship:

create_remote_relationship
--------------------------

``create_remote_relationship`` is used to create a new remote relationship with an existing remote schema.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

  {
    "type": "create_remote_relationship",
    "args": {
      "name": "sample_remote_relationship",
      "table": "users",
      "hasura_fields": [
        "id"
      ],
      "remote_schema": "my-remote-schema",
      "remote_field": {
        "messages": {
          "arguments": {
            "where": {
              "id": {
                "eq": "$id"
              }
            }
          }
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
     - List of PG columns in the table to join the remote schema
   * - remote_schema
     - true
     - :ref:`RemoteSchemaName <RemoteSchemaName>`
     - Name of the remote schema to join with
   * - remote_field
     - true
     - RemoteField_
     - Joining parameters of the remote join

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
      "hasura_fields": [
        "id"
      ],
      "remote_schema": "my-remote-schema",
      "remote_field": {
        "messages": {
          "arguments": {
            "where": {
              "id": {
                "eq": "$id"
              }
            }
          }
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
     - List of PG columns in the table to join the remote schema
   * - remote_schema
     - true
     - :ref:`RemoteSchemaName <RemoteSchemaName>`
     - Name of the remote schema to join with
   * - remote_field
     - true
     - RemoteField_
     - Joining parameters of the remote join

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

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "message": {
          "arguments":{
             "where": {
                 "id":{
                     "eq":"$id"
                  }
             }
          }
       }
   }

Where ``message`` is a top-level node exposed by the remote-schema and the
``arguments`` specifies how to join the specified hasura table to the ``message``
table.
