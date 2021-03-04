.. meta::
   :description: Manage computed fields with the Hasura metadata API
   :keywords: hasura, docs, metadata API, API reference, computed field

.. _metadata_api_computed_field:

Metadata API Reference: Computed Fields (v2.0 and above)
========================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

**computed field** is an extra field added to a table, its value is
computed via an SQL function which has the table row type as an input argument.
Currenty, the Hasura GraphQL engine supports functions returning
`base types <https://www.postgresql.org/docs/current/extend-type-system.html#id-1.8.3.5.9>`__ or
`table row types <https://www.postgresql.org/docs/current/rowtypes.html#ROWTYPES-DECLARING>`__
as computed fields.

.. admonition:: Supported from

  The metadata API is supported for versions ``v2.0.0`` and above and replaces the older
  :ref:`schema/metadata API <schema_metadata_apis>`.

.. _pg_add_computed_field:

pg_add_computed_field
---------------------

``pg_add_computed_field`` is used to define a computed field in a table.
There cannot be an existing column or relationship or computed field with
the same name.

Create a ``computed field`` called ``full_name`` on an ``author`` *table*, using
an SQL function called ``author_full_name``:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type":"pg_add_computed_field",
       "args":{
           "table":{
               "name":"author",
               "schema":"public"
           },
           "source": "default",
           "name":"full_name",
           "definition":{
               "function":{
                   "name":"author_full_name",
                   "schema":"public"
               },
               "table_argument":"author_row"
           }
       }
   }

.. _pg_add_computed_field_syntax:

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
     - :ref:`TableName <TableName>`
     - Name of the table
   * - name
     - true
     - :ref:`ComputedFieldName <ComputedFieldName>`
     - Name of the new computed field
   * - definition
     - true
     - :ref:`ComputedFieldDefinition`
     - The computed field definition
   * - comment
     - false
     - text
     - comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _pg_drop_computed_field:

pg_drop_computed_field
----------------------

``pg_drop_computed_field`` is used to drop a computed field of a table. If
there are other objects dependent on this computed field, like permissions, the request will fail and report the
dependencies unless ``cascade`` is set to ``true``. If ``cascade`` is set to ``true``, the dependent objects
are also dropped.

Drop a computed field ``full_name`` from a table ``author``:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type":"pg_drop_computed_field",
       "args":{
           "table":{
               "name":"author",
               "schema":"public"
           },
           "source": "default",
           "name":"full_name",
           "cascade": false
       }
   }

.. _pg_drop_computed_field_syntax:

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
     - :ref:`TableName <TableName>`
     - Name of the table
   * - name
     - true
     - :ref:`ComputedFieldName <ComputedFieldName>`
     - Name of the computed field
   * - cascade
     - false
     - Boolean
     - When set to ``true``, all the dependent items (if any) on this computed fields are also dropped
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

