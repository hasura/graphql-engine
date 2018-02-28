.. .. meta::
   :description: Hasura's Data microservice's delete query - JSON body's syntax, description, response params and examples.
   :keywords: hasura, docs, data, query reference, delete query

.. _data_delete:

delete
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
   * - where
     - true
     - :ref:`BoolExp <BoolExp>`
     - Count only the rows where this expression holds true
   * - returning
     - false
     - :ref:`PGColumn <PGColumn>` array
     - Return these columns of the deleted rows

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
``delete`` deletes rows that satisfy the ``"where"`` condition from the specified table.

The optional ``"returning"`` key causes ``delete`` to return value(s) based on each row actually deleted. However, you *cannot* request relationships in ``returning``.

You must have ``delete`` permission on the table.  You must also have ``select`` permission on all the columns used in ``"where"`` and ``"returning"``.

On successful completion, an ``delete`` operation returns the number of rows deleted. If the ``delete`` operation contains ``"returning"``, the result will include a key ``"returning"`` whose value will be similar to that of a ``select`` operation with the columns in the ``returning`` list, over the row(s) deleted by the operation.

Example
^^^^^^^

Delete all articles with rating less than 1

.. code-block:: bash

   curl "$HASURADB_URL/v1/query"
     -X POST
     -H "Authorization: auth_code"
     -H "Content-Type: application/json"
     -d '{  "type" : "delete",
            "args" : {
               "table"     : "article",
               "where"     : { "rating" : { "$lt" : 1 } },
               "returning" : ["id"]
            }
         }'

The response looks like::

  {
      "affected_rows" : 2,
      "returning" : [
          { "id" : 21 },
          { "id" : 9 }
      ]
  }
