.. meta::
   :description: Use Postgres functions with Hasura
   :keywords: hasura, docs, postgres, functions, stored procedures

.. _postgres_functions:

Postgres functions
==================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

`Postgres functions <https://www.postgresql.org/docs/current/sql-createfunction.html>`__ allow you to customize your database
schema by defining a set of operations that can include several statements such as declarations, assignments and conditional workflows. Postgres
functions are similar to views but allow more procedural computations and can take arguments. SQL functions are also referred to as **stored procedures**.

.. note::

  For more information on Postgres functions, please refer to the `Postgres documentation <https://www.postgresql.org/docs/current/sql-createfunction.html>`__.

Examples
--------

**Searching articles**

We can create the following function that we can call later to search articles based on the input text argument ``search``.

.. code-block:: plpgsql

  CREATE FUNCTION search_articles(search text)
  RETURNS SETOF article AS $$
      SELECT *
      FROM article
      WHERE
        title ilike ('%' || search || '%')
        OR content ilike ('%' || search || '%')
  $$ LANGUAGE sql STABLE;

Let's break this function apart:

- Function name: ``search_articles``
- Parameters: there is one parameter where ``search`` is the name and ``text`` is the type
- Return type: ``SETOF article``
- Function body: Block from ``SELECT`` until the end of the ``WHERE`` clause
- Language: The response is returned in the ``sql`` language
 
Postgres functions & Hasura
---------------------------

Postgres functions can be exposed in Hasura's GraphQL schema as a top-level field or as a computed field for a table. They are typically used for performing custom business logic in the database.

Refer to :ref:`Custom SQL functions <pg_custom_sql_functions>` and :ref:`Computed fields <pg_computed_fields>` for more use cases and for instructions on how to create and expose Postgres functions in Hasura.
