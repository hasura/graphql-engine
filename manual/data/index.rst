.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Data
====================

The data APIs can be used to store and retrieve data. The actual data is stored in Postgres tables. To fetch associated data, one can define relationships on tables. Permissions can then be used to authorize the access to data based on roles.

Tables
--------

1. Using the console UI. Head to ``Data > Schema``.

   .. image:: ../../img/manual/data/create_table_console.png


2. You can also create tables using SQL by heading to ``Data>SQL`` section in the console.

   .. image:: ../../img/manual/data/create_table_console_sql.png

3. or using the API:

   .. code-block:: http

      POST data.<project-name>.hasura-app.io/v1/query HTTP/1.1
      Authorization: Bearer <admin-token>
      Content-Type: application/json

      {
          "type" : "run_sql",
          "args" : {
              "sql" : "CREATE TABLE category (
                           id SERIAL NOT NULL PRIMARY KEY,
                           name TEXT NOT NULL
                       );"
          }
      }

Inserting data
----------------

Let's insert a couple of categories. The full definition of `insert` request can be found :ref:`here <data_insert>`.

.. code-block:: http
   :emphasize-lines: 13

   POST data.<project-name>.hasura-app.io/v1/query HTTP/1.1
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

Retrieving data
------------------

The JSON based query language lets you make simple to complex queries.

Let's look at a simple `select` query on the article table. The full definition of a `select` query can be found :ref:`here <data_select>`

.. code-block:: http

   POST data.<project-name>.hasura-app.io/v1/query HTTP/1.1
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

   POST data.<project-name>.hasura-app.io/v1/query HTTP/1.1
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

   POST data.<project-name>.hasura-app.io/v1/query HTTP/1.1
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

Update queries
------------------

The request to update data consists of two parts - the new values and a ``where`` indicating what rows to update. The syntax of where clause is same as in the `select` query. For the full syntax of update request, see :ref:`here <data_update>`.

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

Delete queries
-------------------

The request to delete data takes a ``where`` clause indicating what to delete. The syntax of where clause is same as in the `select` query. For the full syntax of delete request, see :ref:`here <data_delete>`.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json

   {
       "type" : "delete",
       "args" : {
           "table" : "article",
           "where": {
              "rating": { "$lte" : 1 }
           }
       }
   }

Relationships
---------------

There is usually some kind of association between tables in a database. These associations are typically captured by foreign key constraints when the data is modelled. The data APIs lets you define relationshps based on these foreign key constraints.

For example, an ``article`` table might have a colmun called ``category_id`` which points to a row in the ``category`` table. Because of this, you may wish to fetch the *articles* of each category when fetching categories, or fetch the *category* of an article when fetching articles.

These additional properies, *articles* of a category and *category* of an article, made possible because of foreign key constraints are what we call relationships. *articles* of category is an array relationship while *category* of article is an object relationship.

Head to the Relationships section of a table in the console. It'll suggest you the appropriate relationships based on the foregin key constraints that the table is involved in.

To obtain the **author**'s name from the article table, we issue,

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "select",
       "args" : {
           "table" : "article",
           "columns": [
               "title",
               {
                   "name": "author",
                   "columns": ["name"]
               }
           ],
           "where" : {"is_published" : true}
       }
   }

The same syntax can be used to obtain the titles of all articles across all **authors**.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "select",
       "args" : {
           "table" : "author",
           "columns": [
               "name",
               {
                   "name": "articles",
                   "columns": ["title"]
               }
           ]
       }
   }

You can use relationships inside ``where`` clause. For example, if we wish to only fetch all published articles by author with name ``Warren`` , we could :

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "select",
       "args" : {
           "table" : "article",
           "columns": [ "id", "title"],
           "where" : {
               "is_published" : true,
               "author" : {
                   "name" : "Warren"
               }
           }
       }
   }

Let's fetch authors who have never published anything.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "select",
       "args" : {
           "table" : "author",
           "columns": ["name"],
           "where" : {
               "$not" : {
                   "articles" : { "$any" : { "is_published" : true }}
               }
           }
       }
   }

As you probably guessed, relationships can be nested. Let's get all published articles with author information, comments and the author who posted the comment.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "select",
       "args" : {
           "table" : "article",
           "columns": [
               "title",
               {
                   "name": "author",
                   "columns": ["name"]
               },
               {
                   "name" : "comments",
                   "columns" : [
                       "comment",
                       {
                           "name" : "commented_by",
                           "columns" : ["name"]
                       }
                   ]
               }
           ],
           "where" : {"is_published" : true}
       }
   }

We can also use ``where``, ``limit``, ``offset`` inside array relationships. Let's say we want to fetch all authors and only their published articles:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "select",
       "args" : {
           "table" : "author",
           "columns": [
               "name",
               {
                   "name": "articles",
                   "columns": ["title"],
                   "where" : { "is_published" : true }
               }
           ]
       }
   }

Aggregations
---------------


The JSON based query language in ``select`` query is designed to be simple yet powerful. There will be queries that you cannot express with the ``select`` query. For example, getting the number of likes for each article. Aggregations (like counting the number of likes) are not supported in the ``select`` query syntax. This is conscious decision we've made to keep the query language small.

To express complex queries like aggregations, window functions, custom joins etc, use SQL, which is designed for this purpose. If you can express your query in SQL, define a view with it, you can use the ``data`` APIs on these views.

Let's see how we can get the likes of an article.

Let us define a view in `SQL <https://www.postgresql.org/docs/current/static/sql-createview.html>`_:

.. code-block:: sql

   CREATE VIEW article_like_count AS
      SELECT article_id, COUNT(author_id) AS like_count
      FROM article_like
      GROUP BY article_id;

.. code-block:: http

   POST data.<project-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
     "type" : "run_sql",
     "args" : {
       "sql" : "CREATE VIEW article_like_count AS...",
     }
   }

Let us then add this view, using the ``add_existing_table_or_view`` query type:

.. code-block:: http

   POST data.<project-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
     "type" : "add_existing_table_or_view",
     "args" : {
       "name" : "article_like_count"
     }
   }

As soon as the view is tracked by the Data API, you can use ``select`` as if ``article_like_count`` is a table.

.. admonition:: Views are read only!

   Views are like read-only logical tables on the database.
   So that means that Data API requests to select will work, but you cannot
   insert/update/delete items from the view.

We've seen how we can get article`s likes using the ``data`` APIs. However, this additional information of each article can be attached to the article table using an object relationship say, ``article_like_count``.

All the relationships that we've defined till now use foreign key constraints. However, you cannot define foreign key constraints on/to views. So, in these cases, we can manually define a relationship as follows:

.. code-block:: http
   :emphasize-lines: 11-15

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type": "create_object_relationship",
       "args": {
           "table": "article",
           "name": "article_like_count",
           "using": {
               "manual_configuration" : {
                   "remote_table" : "article_like_count",
                   "column_mapping" : {
                       "id" : "article_id"
                   }
               }
           }
       }
   }

Let's fetch articles ordered by the number of likes.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "select",
       "args" : {
           "table" : "article",
           "columns": [
               "id", "title",
               {
                   "name" : "article_like_count",
                   "columns" : ["like_count"]
               }
           ],
           "where": {"is_published": true},
           "order_by" : "+article_like_count.like_count"
       }
   }
