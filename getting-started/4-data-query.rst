.. meta::
   :description: Part 4 of a set of learning exercises meant for exploring Hasura in detail. This part shows you how to consume the data service's instant JSON API.
   :keywords: hasura, getting started, step 4, data API

========================================
Part IV: Using the instant JSON data API
========================================

By adding tables to Postgres via the console, or via the data API,
the ``data`` service is able to provide API for performing CRUD queries
across all the defined tables.

Let's see the API in action. The ``data`` API, by default is only open for the ``admin`` role. So, do not forget to include the admin token with the requests. We'll see how to allow access to other roles in the coming sections.

--------------------------------------------------------------------------

Importing seed data
===================

Let's import some data into our tables. Since, Postgres is the underlying database, we need to import data into Postgres. Let's see how we can access the Postgres service running on your project.

Head over to the Advanced settings page from your console. Under "Authorized SSH Keys", add your public key.  Under "Accessing Services", you can see a SSH command as follows:

.. code-block:: console

   $ ssh -p 22 -L 5432:postgres.hasura:5432 hasura@ssh.<project-name>.hasura-app.io

Once, you run the above command you can access the postgres service at ``127.0.0.1:5432``. We can then import data as follows:

.. code-block:: console

   $ psql -h localhost -p 5432 -U admin -d hasuradb  < blog-random-data.sql

Simple queries
==============

.. admonition:: Video reference

   `Using Postman to make data API queries <https://www.youtube.com/watch?v=4OG2FEEFTGE>`_

Inserting Data
--------------

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

Querying Data
-------------

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

Exploiting relationships
========================

If our data API service could understand the different relationships in data, then
we would be able to make more complex and interesting queries to fetch data.

.. code-block:: javascript

   // Current output of select query
   [{
      "id": 1,
      "title": "My first article",
      "content": "Lots of content...",
      "author_id": 3
   }]

   // Desired output of select query
   [{
      "id": 1,
      "title": "My first article",
      "content": "Lots of content...",
      "author_id": 3,
      "author": {
         "name": "Ramu"
         "id": 3
      }
   }]


Let's look at the different relationships we have in our data models:

* author:

  * has ``articles`` written by them
  * has ``comments`` posted by them
  * has ``liked_articles`` liked by them

* comment:

  * has an ``author`` who is the poster
  * has an ``article`` on which it was posted

* article:

  * has an ``author``
  * has ``comments``

* like:

  * has an ``author`` who has liked
  * has an ``article`` that has been liked

These relationships are captured by foreign key constraints where possible. If we were to represent rows of our table in JSON, as objects, then we can express these relationships as nested arrays or objects. Eg: Every ``author`` object can have a key called ``articles`` which is an array of article objects. Similarly, every ``article`` object can have a key called ``author`` which is an author object.

Let's see how these relationships are established.

.. list-table::
   :header-rows: 1

   * - Table
     - Relationship
     - Type
     - Established by
   * - author
     - articles
     - array
     - ``article(author_id) -> blog_user(hasura_id)``
   * - author
     - comments
     - array
     - ``comment(author_id) -> blog_user(hasura_id)``
   * - author
     - liked_articles
     - array
     - ``article_like(user_id) -> blog_user(hasura_id)``

   * - article
     - author
     - object
     - ``article(author_id) -> blog_user(hasura_id)``
   * - article
     - comments
     - array
     - ``comment(article_id) -> article(id)``
   * - article
     - categories
     - array
     - ``category(article_id) -> article(id)``

   * - article_like
     - liked_by
     - object
     - ``article_like(user_id) -> blog_user(hasura_id)``
   * - article_like
     - article
     - object
     - ``article_like(article_id) -> article(id)``

   * - comment
     - commented_by
     - object
     - ``comment(author_id) -> blog_user(hasura_id)``
   * - comment
     - article
     - object
     - ``comment(article_id) -> article(id)``

   * - category
     - articles
     - array
     - ``article_category(article_id) -> article(article_id)``

   * - article_category
     - article
     - object
     - ``article_category(article_id) -> article(id)``
   * - article_category
     - category
     - object
     - ``article_category(category_id) -> category(id)``

Creating relationships
----------------------

You can create relationship metadata for tables via the console, or via the data APIs.

Option 1: Console
^^^^^^^^^^^^^^^^^
Head to ``console > data > Tables > article > modify table`` and scroll down to the relationships section.
Watch this video to see how all the relationships are created via the UI.

Option 2: Relationship creation API
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type": "bulk",
       "args": [
           {
               "type": "create_array_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": {
                           "column": "author_id",
                           "table": "article"
                       }
                   },
                   "table": "author",
                   "name": "articles"
               }
           },
           {
               "type": "create_array_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": {
                           "column": "author_id",
                           "table": "comment"
                       }
                   },
                   "table": "author",
                   "name": "comments"
               }
           },
           {
               "type": "create_array_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": {
                           "column": "author_id",
                           "table": "article_like"
                       }
                   },
                   "table": "author",
                   "name": "liked_articles"
               }
           },
           {
               "type": "create_object_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": "author_id"
                   },
                   "table": "article",
                   "name": "author"
               }
           },
           {
               "type": "create_array_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": {
                           "column": "article_id",
                           "table": "comment"
                       }
                   },
                   "table": "article",
                   "name": "comments"
               }
           },
           {
               "type": "create_array_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": {
                           "column": "article_id",
                           "table": "article_category"
                       }
                   },
                   "table": "article",
                   "name": "categories"
               }
           },
           {
               "type": "create_object_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": "author_id"
                   },
                   "table": "article_like",
                   "name": "liked_by"
               }
           },
           {
               "type": "create_object_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": "article_id"
                   },
                   "table": "article_like",
                   "name": "article"
               }
           },
           {
               "type": "create_object_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": "author_id"
                   },
                   "table": "comment",
                   "name": "commented_by"
               }
           },
           {
               "type": "create_object_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": "article_id"
                   },
                   "table": "comment",
                   "name": "article"
               }
           },
           {
               "type": "create_array_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": {
                           "column": "category_id",
                           "table": "article_category"
                       }
                   },
                   "table": "category",
                   "name": "articles"
               }
           },
           {
               "type": "create_object_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": "article_id"
                   },
                   "table": "article_category",
                   "name": "article"
               }
           },
           {
               "type": "create_object_relationship",
               "args": {
                   "using": {
                       "foreign_key_constraint_on": "category_id"
                   },
                   "table": "article_category",
                   "name": "category"
               }
           }
       ]
   }



Queries using relationships
---------------------------

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
