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

Introduction
------------

A `Postgres view <https://www.postgresql.org/docs/current/sql-createview.html>`__ is a virtual table in Postgres. It represents the result of a query to one or more underlying tables in Postgres.
Views are used to simplify complex queries since these queries are defined once in the view, and can then be directly queried via the same.

.. note::

  Please refer to the Postgres documentation for more details on `standard views <https://www.postgresql.org/docs/current/sql-createview.html>`__ and `materialized views <https://www.postgresql.org/docs/current/rules-materializedviews.html>`__.

Standard views
--------------

`Standard views <https://www.postgresql.org/docs/current/sql-createview.html>`__ represent the result of a query without actually storing data.

Examples
********

**View with authors whose rating is larger than 6:**

.. code-block:: sql

  CREATE VIEW popular_authors AS
    SELECT name, rating
    FROM authors
    WHERE rating > 6;

The created view can now be queried as follows:

.. code-block:: sql

  SELECT name, rating from popular_authors;

**View with authors ordered by their rating:**

.. code-block:: sql

  CREATE VIEW authors_ordered_by_rating AS
      SELECT name, rating
      FROM authors
      ORDER BY rating;

The created view can now be queried as follows:

.. code-block:: sql

  SELECT name, rating from authors_ordered_by_rating;

Materialized views
------------------

Compared to the standard view described above, `materialized views <https://www.postgresql.org/docs/current/rules-materializedviews.html>`__ **do** store data physically in the database.
Materialized views are used if data from complex queries needs to be accessed quickly. 

Example
*******

.. _pg_materialized_view_example:

**Materialized view with authors whose rating is larger than 6 and who are active, ordered by rating:**

.. code-block:: sql

  CREATE MATERIALIZED VIEW popular_active_authors AS
      SELECT name, rating
      FROM authors
      WHERE rating > 6 AND is_active = TRUE
      ORDER BY rating;

The created materialized view can now be queried as follows:

.. code-block:: sql

  SELECT name, rating from popular_active_authors;

Refreshing materialized views
*****************************

Materialized views don't always have the most recent data. 
Since the result of a query is stored in a materialized view like in a cache, you need to make sure to refresh it periodically:

.. code-block:: sql

  REFRESH MATERIALIZED VIEW popular_active_authors;

Materialized views can be refreshed when their underlying source data changes using :ref:`Postgres triggers <postgres_triggers>`.


Postgres views & Hasura
-----------------------

After creating a view, you can expose it over your GraphQL API and query it like a normal table.

See :ref:`this page <custom_views>` for more info on how to create and expose views in Hasura.
