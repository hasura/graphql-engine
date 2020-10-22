.. meta::
   :description: Customise the Hasura GraphQL schema with views
   :keywords: hasura, docs, schema, view

.. _custom_views:

Extend schema with views
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:


What are views?
---------------

`Views <https://www.postgresql.org/docs/current/sql-createview.html>`__ can be used to expose the results of a custom
query as a virtual table. Views are not persisted physically i.e. the query defining a view is executed whenever
data is requested from the view.

Hasura GraphQL engine lets you expose views over the GraphQL API to allow querying them using both ``queries`` and
``subscriptions`` just like regular tables.

.. _create_views:

Creating & exposing views
-------------------------

Views can be created using SQL which can be run in the Hasura console:

- Head to the ``Data -> SQL`` section of the Hasura console
- Enter your `create view SQL statement <https://www.postgresql.org/docs/current/static/sql-createview.html>`__
- Select the ``Track this`` checkbox to expose the new view over the GraphQL API
- Hit the ``Run`` button

Use cases
---------

Views are ideal solutions for retrieving some derived data based on some custom business logic. If your custom logic
requires any user input, you should use :ref:`custom SQL functions <custom_sql_functions>` instead.

Let's look at a few example use cases for views:

Example: Group by and then aggregate
************************************

Sometimes we might want to fetch some data derived by aggregating (avg, min, max, etc.) over a group of rows in a table.

Let’s say we want to fetch the average article rating for each author in the following schema:

.. code-block:: plpgsql

  author(id integer, name text, city text, email text, phone integer, address text)

  article(id integer, title text, content text, rating integer, author_id integer)

A view that averages the rating of articles for each author can be created using the following SQL query:

.. code-block:: SQL

  CREATE VIEW author_average_rating AS
    SELECT author_id, avg(rating)
      FROM article
      GROUP BY author_id


Example: Hide certain fields of a table
***************************************

Sometimes we might have some sensitive information in a table which we wouldn't want to expose.

Let's say, we want to expose the following ``author`` table without the fields ``email``, ``phone`` and ``address``:

.. code-block:: plpgsql

  author(id integer, name text, city text, email text, phone integer, address text)

A view that only exposes the non-sensitive fields of the ``author`` table can be created using the following SQL query:

.. code-block:: SQL

  CREATE VIEW author_public AS
    SELECT id, name, city
      FROM author
