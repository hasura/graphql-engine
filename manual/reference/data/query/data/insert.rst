.. .. meta::
   :description: Hasura's Data microservice's insert query - JSON body's syntax, description, response params and examples.
   :keywords: hasura, docs, data, query reference, insert query


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
   * - on_conflict
     - false
     - ConflictClause_
     - The action to take on a unique constraint violation error (used for upsert)
   * - returning
     - false
     - :ref:`PGColumn <PGColumn>` array
     - Return these columns of the inserted rows

.. _ConflictClause:

``ConflictClause``
&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - action
     - true
     - One of ``update`` or ``ignore``
     - The action to be executed
   * - constraint
     - false
     - Constraint Name
     - Name of the unique constraint which is violated
   * - constraint_on
     - false
     - :ref:`PGColumn <PGColumn>` or :ref:`PGColumn <PGColumn>` array
     - Unique constraint on the specified column(s) is violated

.. note:: One (and only one) of ``constraint`` or ``constraint_on`` has to be present when the action is ``update``. They
   are optional when the action is ``ignore``.

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

``on_conflict`` can be used to specify an alternative action (update or ignore) to take on a unique constraint violation error. This functionality can be used to write an ``upsert`` query (insert a row into a table but update it if it already exists). The update action is only allowed if the ``allow_conflict_update`` is set to ``true`` in the insert permission.

The optional ``returning`` key causes ``insert`` to return value(s) based on each row actually inserted. This is primarily useful for obtaining values that were supplied by defaults, such as a serial sequence number. However, you *cannot* request relationships in ``returning``.

You must have ``insert`` permission on a table in order to insert into it. Use of the ``returning`` requires ``select`` permission on all columns mentioned in ``returning``.

On successful completion, an ``insert`` operation returns the number of rows inserted. If the ``insert`` operation contains ``"returning"``, the result will include a key ``returning`` whose value will be similar to that of a ``select`` operation with the columns in the ``returning`` list, over the row(s) inserted by the operation.

Examples
^^^^^^^^

A simple insert query returning the auto-incremented ``id`` of the inserted rows.

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <token>

   {
       "type" : "insert",
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
   }

The response looks like::

  {
      "affected_rows" : 2,
      "returning" : [
          { "id" : 1 },
          { "id" : 2 }
      ]
  }


An upsert query where there is a unique constraint on ``("user_id", "article_id")`` columns.

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <token>

   {
       "type" : "insert",
       "args" : {
           "table"     : "article_rating",
           "objects"   : [
             {
               "user_id" : 1,
               "article_id" : 1,
               "rating" : 3
             },
             {
               "user_id" : 1,
               "article_id" : 2,
               "rating" : 4
             }
           ],
           "on_conflict" : {
               "action" : "update",
               "constraint_on" : ["user_id", "article_id"]
           }
      }
   }

The response looks like::

  {
      "affected_rows" : 2
  }

In the above query, if a row already exists, which is determined by the unique constraint violation on (``article_id``, ``user_id``), then the row is updated with the new rating. If the row does not exist, it is inserted.
