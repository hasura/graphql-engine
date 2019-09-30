Schema/Metadata API Reference: Computed Columns
===============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

**Computed column** is an extra field added to a table, its value is
computed via an SQL function which has the table row type as an input argument.
Currenty, the Hasura GraphQL Engine supports functions returning
`Base types <https://www.postgresql.org/docs/current/extend-type-system.html#id-1.8.3.5.9>`__ or
`Table row types <https://www.postgresql.org/docs/current/rowtypes.html#ROWTYPES-DECLARING>`__
as computed columns.

.. _add_computed_column:

add_computed_column
-------------------

``add_computed_column`` is used to define a computed column in a table.
There cannot be an existing column or relationship or computed column with
the same name.

Create a ``computed column`` ``get_articles`` on ``author`` *table*, using
SQL function ``fetch_articles``:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type":"add_computed_column",
       "args":{
           "table":{
               "name":"author",
               "schema":"public"
           },
           "name":"get_articles",
           "definition":{
               "function":{
                   "name":"fetch_articles",
                   "schema":"public"
               },
               "table_argument":"author_row"
           }
       }
   }

.. _add_computed_column_syntax:

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
     - :ref:`ComputedColumnName <ComputedColumnName>`
     - Name of the new computed column
   * - definition
     - true
     - Definition_
     - The computed column definition
   * - comment
     - false
     - text
     - comment

.. _Definition:

Definition
&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - function
     - true
     - :ref:`FunctionName <FunctionName>`
     - The SQL function
   * - table_argument
     - false
     - String
     - Name of argument which accepts table row type. If omitted the first
       argument is considered as table argument

.. _drop_computed_column:

drop_computed_column
--------------------

``drop_computed_column`` is used to drop a computed column of a table. If
there are other objects dependent on this computed column like permissions
etc., the query will fail and report the dependencies unless ``cascade`` is
set to ``true``. If ``cascade`` is set to ``true``, the dependent objects
are also dropped.

Drop a computed column ``get_articles`` from a table ``author``:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type":"drop_computed_column",
       "args":{
           "table":{
               "name":"author",
               "schema":"public"
           },
           "name":"get_articles",
           "cascade": false
       }
   }

.. _drop_computed_column_syntax:

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
     - :ref:`ComputedColumnName <ComputedColumnName>`
     - Name of the computed column
   * - cascade
     - false
     - Boolean
     - When set to ``true``, all the dependent items (if any) on this computed columns also dropped
