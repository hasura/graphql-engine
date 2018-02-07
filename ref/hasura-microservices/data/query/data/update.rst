.. meta::
   :description: Hasura's Data service's update query - JSON body's syntax, description, response params and examples.
   :keywords: hasura, docs, data, query reference, update query

.. _data_update:

update
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
   * - $set
     - false
     - Object (:ref:`PGColumn <PGColumn>` : Value)
     - Sets the columns to the corresponding values. Note that each value has to be compatible with the type of the corresponding column
   * - $inc
     - false
     - Object (:ref:`PGColumn <PGColumn>` : Integer)
     - Increments the columns with the corresponding values
   * - $mul
     - false
     - Object (:ref:`PGColumn <PGColumn>` : Integer)
     - Multiplies the columns with the corresponding values
   * - $default
     - false
     - :ref:`PGColumn <PGColumn>` array
     - Sets these columns to their default values
   * - where
     - true
     - :ref:`BoolExp <BoolExp>`
     - Update only the rows where this expression holds true
   * - returning
     - false
     - :ref:`PGColumn <PGColumn>` array
     - Return these columns of the deleted rows

.. note:: At least one of ``$set``, ``$inc``, ``$mul``, ``$default`` should be present

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

``update`` changes the values of the specified columns in all rows that satisfy the condition. The columns whose values have to be updated should be given in any of ``$set``, ``$inc``, ``$mul`` or ``$default``.

The optional ``"returning"`` key causes ``update`` to return value(s) based on each row actually updated. However, you *cannot* request relationships in ``returning``.

You must have ``update`` permission on the table *and* on the columns that are given in ``"values"``.  You must also have ``select`` permission on all the columns used in ``"where"`` and ``"returning"``.

On successful completion, an ``update`` operation returns the number of rows updated. If the ``update`` operation contains ``"returning"``, the result will include a key ``"returning"`` whose value will be similar to that of a ``select`` operation with the columns in the ``returning`` list, over the row(s) updated by the operation.

Example
^^^^^^^

Mark the user with id ``2`` as verified.

.. code-block:: bash

   curl "$HASURADB_URL/v1/query" \
     -X POST \
     -H "Authorization: Bearer <authorization-token>" \
     -H "Content-Type: application/json" \
     -d '{ "type" : "update", \
           "args" : {
             "table"    : "user",
             "$set"      : { "verified" : true },
             "where"     : { "id" : 2 },
             "returning" : ["id"]}
         }'

The response looks like::

  {
      "affected_rows" : 1,
      "returning" : [
          { "id" : 2 }
      ]
  }
