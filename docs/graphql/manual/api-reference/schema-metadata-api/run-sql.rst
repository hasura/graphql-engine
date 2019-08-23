Schema/Metadata API Reference: Run SQL
======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

.. _run_sql:

run_sql
-------

``run_sql`` can be used to run arbitrary SQL statements.

Multiple SQL statements can be separated by a ``;``, however, only the result of the last SQL statement will be
returned.

.. admonition:: Admin-only

  This is an admin-only query, i.e. the query can only be executed by a
  request having ``X-Hasura-Role: admin``. This can be set by passing
  ``X-Hasura-Admin-Secret`` or by setting the right role in Webhook/JWT
  authorization mode.

  This is deliberate as it is hard to enforce any sort of permissions on arbitrary SQL. If
  you find yourselves in the need of using ``run_sql`` to run custom DML queries,
  consider creating a view. You can now define permissions on that particular view
  for various roles.

Use cases
^^^^^^^^^

1. To execute DDL operations that are not supported by the console (e.g. managing indexes).
2. Run custom DML queries from backend microservices instead of installing libraries to speak to Postgres.

An example:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "run_sql",
       "args": {
           "sql": "CREATE UNIQUE INDEX ON films (title);"
       }
   }

While ``run_sql`` lets you run any SQL, it tries to ensure that the Hasura GraphQL engine's
state (relationships, permissions etc.) is consistent. i.e., you
cannot drop a column on which any metadata is dependent on (say a permission or
a relationship). The effects, however, can be cascaded.

Example:- If we were to drop 'bio' column from the author table (let's say
the column is used in some permission), you would see an error.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "run_sql",
       "args": {
           "sql": "ALTER TABLE author DROP COLUMN name"
       }
   }

.. code-block:: http

   HTTP/1.1 400 BAD REQUEST
   Content-Type: application/json

   {
       "path": "$.args",
       "error": "cannot drop due to the following dependent objects : permission author.user.select"
   }

We can however, cascade these changes.

.. code-block:: http
   :emphasize-lines: 9

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "run_sql",
       "args": {
           "sql": "ALTER TABLE author DROP COLUMN bio",
           "cascade" : true
       }
   }

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
       "result_type": "CommandOk"
   }

With the above query, the dependent permission is also dropped.

Example:- If we were to drop a foreign key constraint from the article table
(let's say the column involved in foreign key is used to define a relationship),
you would see an error.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "run_sql",
       "args": {
           "sql": "ALTER TABLE article DROP CONSTRAINT article_author_id_fkey"
       }
   }

.. code-block:: http

   HTTP/1.1 400 BAD REQUEST
   Content-Type: application/json

   {
       "path": "$.args",
       "error": "cannot drop due to the following dependent objects : constraint article.article_author_id_fkey"
   }

We can however, cascade these changes.

.. code-block:: http
   :emphasize-lines: 9

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "run_sql",
       "args": {
           "sql": "ALTER TABLE article DROP CONSTRAINT article_author_id_fkey",
           "cascade" : true
       }
   }

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
       "result_type": "CommandOk"
   }

With the above query, the dependent permission is also dropped.

In general, the SQL operations that will affect Hasura metadata are:

1. Dropping columns
2. Dropping tables
3. Dropping foreign keys
4. Altering types of columns
5. Dropping SQL functions
6. Overloading SQL functions

In case of 1, 2 and 3 the dependent objects (if any) can be dropped using ``cascade``.
However, when altering type of columns, if any objects are affected, the change
cannot be cascaded. So, those dependent objects have to be manually dropped before
executing the SQL statement. Dropping SQL functions will cascade the functions in
metadata even without using ``cascade`` since no other objects dependant on them.
Overloading tracked SQL functions is not allowed.

Set ``check_metadata_consistency`` field to ``false`` to force server to not consider metadata dependencies.

.. _run_sql_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - sql
     - true
     - String
     - The sql to be executed
   * - cascade
     - false
     - Boolean
     - When set to ``true``, the effect (if possible) is cascaded to any hasuradb dependent objects (relationships, permissions, templates).
   * - check_metadata_consistency
     - false
     - Boolean
     - When set to ``false``, the sql is executed without checking metadata dependencies.

Response
^^^^^^^^

The response is a JSON Object with the following structure.

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - result_type
     - true
     - String
     - One of "CommandOk" or "TuplesOk"
   * - result
     - false
     - ``[[Text]]`` (An array of rows, each row an array of columns)
     - This is present only when the ``result_type`` is "TuplesOk"

.. note::
   The first row in the ``result`` (when present) will be the names of the columns.

Some examples
^^^^^^^^^^^^^

A query returning results.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "run_sql",
       "args": {
           "sql": "select user_id, first_name from author limit 2;"
       }
   }

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
       "result_type": "TuplesOk",
       "result": [
           [
               "user_id",
               "first_name"
           ],
           [
               "1",
               "andre"
           ],
           [
               "2",
               "angela"
           ]
       ]
   }


A query to create a table:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
     "type":"run_sql",
     "args": {
       "sql": "create table item ( id serial,  name text,  category text,  primary key (id))",
       "check_metadata_consistency": false
     }
   }

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "result_type": "CommandOk",
     "result": null
   }
