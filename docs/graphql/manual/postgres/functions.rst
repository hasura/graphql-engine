.. meta::
   :description: Use Postgres functions with Hasura
   :keywords: hasura, docs, postgres, functions

.. _postgres_functions:

Postgres functions
==================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

`Postgres functions <https://www.postgresql.org/docs/current/sql-createfunction.html>`__ allow you to customize your database schema by defining a set of operations that can include several statements such as declarations, assignments and conditional workflows. 
Postgres functions are similar to views but are used when we have arguments.

.. note::

  For more information on Postgres functions, please refer to the `Postgres documentation <https://www.postgresql.org/docs/current/sql-createfunction.html>`__.

Use cases
---------

**Example:**

.. code-block:: plpgsql

  CREATE FUNCTION check_author_active()
      RETURNS trigger AS $BODY$
      DECLARE active_author BOOLEAN;
      BEGIN
      SELECT author.is_active INTO active_author FROM "authors" author WHERE author.id = NEW."author_id";
      IF active_author != TRUE THEN
          RAISE EXCEPTION 'Author must be active';
      END IF;
      RETURN NEW;
      END;
      $BODY$ LANGUAGE plpgsql;

The objective of this function is to check if an author is active before a corresponding article is inserted. 
If the author is not active, an exception is raised and the insertion will fail. If the author is active, the article will be inserted and returned.

Let's break this function apart:

- Function name: ``check_author_active``
- Parameters: This function doesn't have parameters (see ``()``)
- Return type: ``trigger``
- Variable declaration: A variable called ``active_author`` is declared
- Function body: Block between ``BEGIN`` and ``END`` checking if the author for whom the article is to be inserted is active
- Response: The response (``$BODY$``) is returned in the ``slpgsql`` language

Postgres functions & Hasura
---------------------------

Functions can be used to extend your Hasura GraphQL API, and they can also be exposed via the same.

See :ref:`here <create_and_expose_sql_functions>` how to create and expose Postgres functions in Hasura.
