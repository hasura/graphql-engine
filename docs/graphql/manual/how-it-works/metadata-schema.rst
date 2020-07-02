.. meta::
   :description: Hasura metadata catalogue
   :keywords: hasura, docs, metadata catalogue, how it works

.. _hasura_metadata_schema:

Metadata catalogue
==================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

What is the metadata catalogue?
-------------------------------

The Hasura metadata catalogue is a set of internal tables used to manage the state of the database and the
GraphQL schema. Hasura GraphQL engine uses the data in the catalogue to generate the GraphQL API
which then can be accessed from different clients.

The Hasura GraphQL engine when initialized, creates a schema called ``hdb_catalog`` in the Postgres database and
initializes a few tables under it as described below.

**hdb_catalog** schema
----------------------

This schema is created by the Hasura GraphQL engine to manage its internal state. Whenever a
table/permission/relationship is created/updated using the Hasura console or the metadata API, the Hasura GraphQL engine
captures that information and stores it in the corresponding tables.

The following tables are used by the Hasura GraphQL engine:

**hdb_table** table
^^^^^^^^^^^^^^^^^^^

This table stores information about all the tables/views which are created/tracked using the Hasura console or
the metadata API.

Schema
""""""

.. thumbnail:: /img/graphql/manual/engine-internals/hdb_table.jpg
  :width: 30%
  :alt: hdb_table schema

Column Definitions
""""""""""""""""""
+---------------------+------------------------------------------------------------------------------------------+
| column              | description                                                                              |
+=====================+==========================================================================================+
| table_schema        | Captures information about the schema under which a table/view is tracked                |
+---------------------+------------------------------------------------------------------------------------------+
| table_name          | Captures name of the tracked table/view.                                                 |
+---------------------+------------------------------------------------------------------------------------------+
| is_system_defined   | If it is true, then the table/view is created by GraphQL engine for internal purpose.    |
|                     | If it is false, then the table/view is created by the end user.                          |
+---------------------+------------------------------------------------------------------------------------------+

**hdb_relationship** table
^^^^^^^^^^^^^^^^^^^^^^^^^^

This table stores information about the relationships created for tables/views using the Hasura console or
the metadata API.

Schema
""""""

.. thumbnail:: /img/graphql/manual/engine-internals/hdb_relationship.jpg
  :width: 30%
  :alt: hdb_relationship schema

Column Definitions
""""""""""""""""""

+---------------------+------------------------------------------------------------------------------------------+
| column              | description                                                                              |
+=====================+==========================================================================================+
| table_schema        | Captures information about the schema under which a relationship is created.             |
+---------------------+------------------------------------------------------------------------------------------+
| table_name          | Captures name of the table/view under which a relationship is created.                   |
+---------------------+------------------------------------------------------------------------------------------+
| rel_na me           | Captures name of the relationship.                                                       |
+---------------------+------------------------------------------------------------------------------------------+
| rel_type            | Captures the permission type (insert/select/update/delete).                              |
+---------------------+------------------------------------------------------------------------------------------+
| perm_def            | Captures information about how the relationship is defined.                              |
|                     |                                                                                          |
|                     | For example:                                                                             |
|                     |                                                                                          |
|                     | .. code-block:: json                                                                     |
|                     |                                                                                          |
|                     |   {                                                                                      |
|                     |     "foreign_key_constraint_on": "user_id"                                               |
|                     |   }                                                                                      |
+---------------------+------------------------------------------------------------------------------------------+
| comment             | Captures the comment for the relationship.                                               |
+---------------------+------------------------------------------------------------------------------------------+
| is_system_defined   | If it is true, then the relationship is created by GraphQL engine for internal purpose.  |
|                     | If it is false, then the relationship is created by the end user.                        |
+---------------------+------------------------------------------------------------------------------------------+


**hdb_permission** table
^^^^^^^^^^^^^^^^^^^^^^^^

This table stores information about the access control rules on tables/views set up using the Hasura console or
the metadata API.

Schema
""""""

.. thumbnail:: /img/graphql/manual/engine-internals/hdb_permission.jpg
  :width: 30%
  :alt: hdb_permission schema

Column Definitions
""""""""""""""""""
+---------------------+------------------------------------------------------------------------------------------+
| column              | description                                                                              |
+=====================+==========================================================================================+
| table_schema        | Captures information about the schema under which a permission is created.               |
+---------------------+------------------------------------------------------------------------------------------+
| table_name          | Captures name of the table/view under which a permission is created.                     |
+---------------------+------------------------------------------------------------------------------------------+
| role_name           | Captures name of the role for which this permission will be applicable.                  |
+---------------------+------------------------------------------------------------------------------------------+
| perm_type           | Captures the permission type (insert/select/update/delete).                              |
+---------------------+------------------------------------------------------------------------------------------+
| perm_def            | Captures information about how the permission is defined.                                |
|                     |                                                                                          |
|                     | Whenever a request is made with the above role for the above table GraphQL engine        |
|                     | will first validate the requested columns with the columns which the user has access to  |
|                     | using the ``columns`` key.                                                               |
|                     | Once the request is validated the appropriate results are returned after applying the    |
|                     | filter defined in the ``filter`` key.                                                    |
|                     |                                                                                          |
|                     | For example:                                                                             |
|                     |                                                                                          |
|                     | .. code-block:: json                                                                     |
|                     |                                                                                          |
|                     |   {                                                                                      |
|                     |     "columns": ["id", "name"],                                                           |
|                     |     "filter": {                                                                          |
|                     |       "id": {                                                                            |
|                     |         "_eq": "X-HASURA-USER-ID"                                                        |
|                     |       }                                                                                  |
|                     |     }                                                                                    |
|                     |   }                                                                                      |
+---------------------+------------------------------------------------------------------------------------------+
| comment             | Captures the comment for the permission.                                                 |
+---------------------+------------------------------------------------------------------------------------------+
| is_system_defined   | If it is true, then the permission is created by GraphQL engine for internal purpose. If |
|                     | it is false, then the permission is created by the end user.                             |
+---------------------+------------------------------------------------------------------------------------------+

.. note::

  This section is a work in progress. There have been other tables and columns added to the catalogue to
  support new features since this was last updated.

Exploring the catalogue
-----------------------

You can check the current schema and contents of the catalogue by exploring the ``hdb_catalog``
schema through a Postgres client.

Catalogue versioning
--------------------

Whenever the schema of the catalogue is modified *(typically to support new features)* a new version of the
catalogue is generated.

The catalogue version is upgraded automatically on startup if a new version is available during Hasura GraphQL engine
updates.
