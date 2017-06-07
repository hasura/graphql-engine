=========================================
Part III: Data modelling for the blog app
=========================================

Our basic data requirements are as follows:

* Storing the profile data of each user
* Storing the article data and the author of each article
* Storing the likes given by users to articles
* Storing the comments written by users on articles

.. admonition:: Video reference

   `Basic data modelling <https://youtu.be/EdBjQ3zRBWg>`_

   The video shows you how to create data models using the project console.
   The document below however, is an API driven approach to creating data models.
   Don't skip reading this guide if you need a solid developer intro to modelling,
   especially if you want to start versioning your schema!


The ``data`` service
====================

Every Hasura project comes with a data service. The ``data`` service provides an HTTP API over PostgreSQL, an extremely versatile open source relational database. We create tables in Postgres and access the data using the APIs provided by the ``data`` service.

Any user with the ``admin`` role has full-access to the data service. All requests to the ``data`` service are ``POST`` requests to ``/v1/query`` endpoint. The body should have two keys ``type`` and ``args``. For example,

.. code-block:: json

   {
       "type": "run_sql",
       "args": {
           "sql" : "some sql"
       }
   }


Creating tables
---------------

As you would normally do with any relational database, data is modelled as tables.

+----------------------------------------+----------------------------------------+
|Table                                   |Columns                                 |
+========================================+========================================+
|author                                  |id, name                                |
+----------------------------------------+----------------------------------------+
|article                                 |id, title, author_id, rating            |
+----------------------------------------+----------------------------------------+
|like                                    |user_id, article_id                     |
+----------------------------------------+----------------------------------------+
|comment                                 |id, user_id, article_id, comment        |
+----------------------------------------+----------------------------------------+

Check out this video to see how we can create tables and model foreign key constraints via console. You can skip to the next section if you use the console.

Let us define the structure of these tables in sql as follows:

.. literalinclude:: blog-schema.sql
   :language: sql

Let's execute this sql on the PostgreSQL database. The ``run_sql`` query type is only allowed for the ``admin`` role. Let's execute the above sql to create the tables.

.. code-block:: http

   POST data.<project-name>.hasura-app.io/v1/query HTTP/1.1
   Authorization: Bearer <admin-token>
   Content-Type: application/json

   {
       "type" : "run_sql",
       "args" : {
           "sql" : "the above sql statements"
       }
   }

The response would be as follows:

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
       "message" : "success"
   }
