.. meta::
   :description: Use Postgres indexes with Hasura
   :keywords: hasura, docs, postgres, indexes

.. _postgres_indexes:

Postgres indexes
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

`Postgres indexes <https://www.postgresql.org/docs/current/sql-createindex.html>`__ are a way of increasing query performance
based on columns that are queried frequently. The concept is similar to the one of an index in a book.
It helps accessing the data you're looking for more quickly by maintaining additional metadata.

.. note::

  Learn more about indexes in the `Postgres documentation <https://www.postgresql.org/docs/current/sql-createindex.html>`__.

Example
-------

**Create an index on the column name in the table authors:**

Let's say the database receives a large number of requests of authors being queried by their name, for example:

.. code-block:: sql

  SELECT * FROM authors WHERE name = 'J.K. Rowling';

We can now create an index on the ``name`` column of the ``authors`` table:

.. code-block:: sql

  CREATE INDEX author_name_index ON authors (name);

Since the database is now able to look up the result of these queries more quickly, the performance of these queries will increase significantly.

Postgres indexes & Hasura
-------------------------

Indexes can be used to optimize query performance in Hasura. :ref:`Refer to this page <data_validation_pg_indexes>` for information about query performance and how to add Postgres indexes to Hasura.
