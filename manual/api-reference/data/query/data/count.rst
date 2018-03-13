.. .. meta::
   :description: Hasura's Data microservice's count query - JSON body's syntax, description, response params and examples.
   :keywords: hasura, docs, data, query reference, count query

.. _data_count:

Data API Reference: count
-------------------------

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
   * - distinct
     - false
     - :ref:`PGColumn <PGColumn>` array
     - Only count the rows with distinct values in these columns
   * - where
     - true
     - :ref:`BoolExp <BoolExp>`
     - Count only the rows where this expression holds true

Response
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - count
     - true
     - Integer
     - The count of the rows defined by the query

Description
^^^^^^^^^^^
``count`` returns a count of the rows that satisfy the ``"where"`` condition from the specified table.

If ``"distinct"`` is specified, all duplicate rows are not counted (one row is counted from each group of duplicates).

You must have ``select`` permission on the table and on all the columns used in ``"where"`` and ``"distinct"``.

Example
^^^^^^^

Count all articles with rating less than 1

.. code-block:: bash

   curl "$HASURADB_URL/v1/query" \
     -X POST \
     -H "Authorization: auth_code" \
     -H "Content-Type: application/json" \
     -d '{ "type" : "count",
           "args" : {
             "table" : "article", "where" : { "rating" : { "$lt" : 1 } }
           }
         }'

The response looks like::

  {
      "count" : 2
  }
