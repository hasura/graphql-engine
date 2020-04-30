.. meta::
   :description: Use Postgres views with Hasura
   :keywords: hasura, docs, postgres, views

.. _postgres_views:

Postgres views
==============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Standard views
--------------

Introduction
************

A `view <https://www.postgresql.org/docs/current/sql-createview.html>`__ is a virtual table in Postgres. It represents the result of a query to one or more underlying tables in Postgres without actually storing data.
Views are used to simplify complex queries since these queries are defined once in the view, and can then be directly queried via the same.

Examples
********

**Example:**

.. code-block:: sql

  CREATE VIEW popular_authors AS
    SELECT name, rating
    FROM authors
    WHERE rating > 6;

This statement creates a view of authors whose rating is larger than 6.

The created view can now be queried as follows:

.. code-block:: sql

  SELECT name, rating from popular_authors;

Materialized views
------------------

Introduction
************

Compared to the standard view described above, `materialized views <https://www.postgresql.org/docs/current/rules-materializedviews.html>`__ **do** store data physically in the database.

Examples
********

**Example:**

.. code-block:: sql

  CREATE MATERIALIZED VIEW popular_authors AS
      SELECT name, rating
      FROM authors
      WHERE rating > 6;

Materialized views are used if data from complex queries needs to be accessed quickly. However, materialized views don't always have the most recent data. 
Since the result of a query is stored in a materialized view like in a cache, you need to make sure to update it periodically:

.. code-block:: sql

  REFRESH MATERIALIZED VIEW popular_authors;

Views & Hasura
--------------

.. note::

  Please refer to the Postgres documentation for more details on `views <https://www.postgresql.org/docs/current/sql-createview.html>`__ and `materialized views <https://www.postgresql.org/docs/current/rules-materializedviews.html>`__.
