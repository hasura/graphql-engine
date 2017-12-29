.. .. meta::
   :description: Part 7 of a set of learning exercises meant for exploring Hasura in detail. This part takes you over data modeling & introduces the data microservice's API.
   :keywords: hasura, getting started, step 3, data modeling

================================
Part VIII: Explore the data APIs
================================

Now that you've created the data models, you can use HTTP JSON APIs to query your data directly.

The ``data`` microservice
-------------------------

Every Hasura project comes with a data microservice. The ``data`` microservice provides an HTTP API over PostgreSQL, an extremely versatile open source relational database. We create tables in Postgres and access the data using the APIs provided by the ``data`` microservice.

Any user with the ``admin`` role has full-access to the data microservice. All requests to the ``data`` microservice are ``POST`` requests to ``/v1/query`` endpoint. The body should have two keys ``type`` and ``args``. For example,

.. code-block:: json

   {
       "type": "run_sql",
       "args": {
           "sql" : "some sql"
       }
   }

Explore using the API explorer & the query builder
--------------------------------------------------

Run:

.. code-block:: bash

   # Run this command inside your project directory
   $ hasura api-console

This will open up the ``api-console`` and show you the ``API explorer`` page which you can use to understand the APIs.

.. admonition:: Note

   You can try out all of the API examples below in the API explorer.
   Head to ``API explorer > Data > ``/v1/query - Query builder``

Inserting Data
--------------

Let's insert a couple of categories. The full definition of `insert` request can be found :ref:`here <data_insert>`.

.. code-block:: http
   :emphasize-lines: 13

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type":"insert",
       "args":{
           "table":"category",
           "objects":[
               {"name":"News"},
               {"name":"Movies"}
           ],
           "returning":["id"]
       }
   }

Note the ``returning`` key. We would like to get the auto incremented id for each inserted row.

Querying Data
-------------

The JSON based query language lets you make simple to complex queries.

Let's look at a simple `select` query on the article table. The full definition of a `select` query can be found :ref:`here <data_select>`

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

Boolean operators like ``$and``, ``$or``, ``$not`` can be used in a ``where`` clause. See :ref:`here <BoolExp>` for a full list of supported Boolean operators.

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

Updating Data
-------------

The request to update data consists of two parts - the new values and a ``where`` indicating what to update. The syntax of where clause is same as in the `select` query. For the full syntax of update request, see :ref:`here <data_update>`.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json

   {
       "type" : "update",
       "args" : {
           "table" : "article",
           "$set": {"title": "Mysterious affair at Styles"},
           "where": {
               "id": 4
           }
       }
   }

Delete Data
-----------

The request to delete data takes a ``where`` clause indicating what to delete. The syntax of where clause is same as in the `select` query. For the full syntax of delete request, see :ref:`here <data_delete>`.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json

   {
       "type" : "delete",
       "args" : {
           "table" : "article",
           "where": {
              "rating": {"$lte" : 1}
           }
       }
   }


Next: Add relationships
------------------------

Next, head to :doc:`Add relationships to your data models <9-adding-relationships>`.
