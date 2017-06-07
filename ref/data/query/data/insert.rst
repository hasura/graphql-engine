.. _data_insert:

insert
------

Syntax
^^^^^^

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
   * - objects
     - true
     - Object array
     - An array of objects, each representing a row that needs to be inserted
   * - returning
     - false
     - :ref:`PGColumn <PGColumn>` array
     - Return these columns of the inserted rows

Response
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - affected_rows
     - true
     - Integer
     - The number of rows which are affected by the query
   * - returning
     - false
     - Object array
     - An array of objects, one per affected row, containing the columns specified in the returning field

Description
^^^^^^^^^^^

``insert`` inserts one or more rows into a table. Each row is specified as a JSON :ref:`Object <Object>`.

Each column not present in the object will be filled with a default value, either its declared default value or null if there is none.

If the value for any column is not of the correct data type, it results in an *error*.

The optional ``returning`` key causes ``insert`` to return value(s) based on each row actually inserted. This is primarily useful for obtaining values that were supplied by defaults, such as a serial sequence number. However, you *cannot* request relationships in ``returning``.

You must have ``insert`` permission on a table in order to insert into it. Use of the ``returning`` requires ``select`` permission on all columns mentioned in ``returning``.

On successful completion, an ``insert`` operation returns the number of rows inserted. If the ``insert`` operation contains ``"returning"``, the result will include a key ``returning`` whose value will be similar to that of a ``select`` operation with the columns in the ``returning`` list, over the row(s) inserted by the operation.

Examples
^^^^^^^^

.. code-block:: bash

   curl "$HASURADB_URL/v1/query" \
     -X POST \
     -H "Authorization: auth_code" \
     -H "Content-Type: application/json" \
     -d '{  "type" : "insert",
            "args" : {
              "table"     : "post",
              "objects"   : [
                {
                  "title"   : "hello world",
                  "content" : "Your first program"
                },
                {
                  "title"   : "foo bar",
                  "content" : "NA"
                }
              ],
              "returning" : ["id"]
            }
         }'

The response looks like::

  {
      "affected_rows" : 2,
      "returning" : [
          { "id" : 1 },
          { "id" : 2 }
      ]
  }
