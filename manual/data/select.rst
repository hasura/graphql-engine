Retrieving data
===============

The JSON based query language lets you make simple to complex queries.

Selecting data
--------------

Let's look at a simple `select` query on the article table.

The full syntax of a ``select`` query can be found :ref:`here <data_select>`.

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "select",
       "args" : {
           "table" : "article",
           "columns": ["id", "title"],
           "where": {"is_published": true}
       }
   }

This query returns ``id`` and ``title`` of rows from ``article`` table where ``is_published`` is ``true``.

.. admonition:: Syntactic sugar

   .. code-block:: json

      { "is_published": true }

   is just a shortcut for writing the 'is-equal-to' operator, ``$eq``

   .. code-block:: json

     { "is_published": { "$eq": true } }

Boolean operators like ``$and``, ``$or``, ``$not`` can be used in a ``where`` clause. See :ref:`this <BoolExp>` for a full list of supported Boolean operators.

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "select",
       "args" : {
           "table" : "article",
           "columns": ["id", "title"],
           "where": {
               "$and" : [
                   {"is_published": true},
                   {"author_id" : 6}
               ]
           }
       }
   }

.. admonition:: Syntactic sugar

   The ``where`` clause in the above query can be simplified as :

   .. code-block:: json

     {
         "is_published": true,
         "author_id" : 6
     }

``order_by`` is used to sort the results by a column. A prefix of ``+`` or ``-`` indicates ascending or descending order respectively. ``limit`` and ``offset`` are used to slice the result set.

Example,

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "select",
       "args" : {
           "table" : "article",
           "columns": ["id", "title"],
           "where": {"is_published": true},
           "order_by" : "+author_id",
           "limit" : 10
       }
   }

Counting data
-------------

Now let's look at a simple `count` query on the article table.

The full syntax of a ``count`` query can be found :ref:`here <data_count>`.

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "count",
       "args" : {
           "table" : "article",
           "where": {"is_published": true}
       }
   }

This query returns the count of rows from ``article`` table where ``is_published`` is ``true``.


