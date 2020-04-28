.. meta::
   :description: Use Postgres indexes with Hasura
   :keywords: hasura, docs, postgres, indexes

.. _indexes:

Postgres indexes
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:
  

`Indexes <https://www.postgresql.org/docs/9.1/sql-createindex.html>`_ are a way of increasing performance on a field that is queried frequently. The concept is similar to the one of an index in a book. 
It helps accessing the data you're looking for more quickly.

**Example:**

Let's say the database receives a large number of requests where an author is queried by their name, for example:

.. code-block:: sql

  SELECT * FROM authors WHERE name = 'J.K. Rowling';

We can now create an index on the ``name`` field of the ``authors`` table:

.. code-block:: sql

  CREATE INDEX author_name_index ON authors (name);

Since the database is now able to look up the result of these queries more quickly, the performance of these queries increases significantly.

.. note::

  Learn more about creating indexes in the `Postgres documentation <https://www.postgresql.org/docs/9.1/sql-createindex.html>`_.
