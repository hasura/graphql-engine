.. meta::
   :description: Customise the Hasura GraphQL schema with views
   :keywords: hasura, docs, schema, view

.. _custom_views:

Customise schema with views
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:


What are views?
---------------

`Views <https://www.postgresql.org/docs/current/sql-createview.html>`_ can be used to expose the results of a custom
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

Let's see a few example use cases for views:

Example: Group by and then aggregate
************************************

Let’s see how to fetch the average article rating for each author in our author/article schema.

A view that averages the rating of articles for each author can be created using the following SQL query:

.. code-block:: SQL

  CREATE VIEW author_average_rating AS
    SELECT author_id, avg(rating)
      FROM article
      GROUP BY author_id


Example: Hide certain fields of a table
***************************************

Say, we have some sensitive information in a table which we wouldn't want to expose. We can create a view that only
exposes the non-sensitive fields.

Let's say our ``author`` table has the fields ``id, name, city, email, phone, address`` and we want to hide the ``email``,
``phone`` and ``address`` fields. We can create the following view to achieve this:

.. code-block:: SQL

  CREATE VIEW author_public AS
    SELECT id, name, city
      FROM author
