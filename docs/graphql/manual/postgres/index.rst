.. meta::
   :description: Manage remote schemas with Hasura
   :keywords: hasura, docs, remote schema

.. _postgres:

Postgres Reference
==================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:


This page is a Postgres reference demonstrating the most common Postgres features that you can use to enhance your GraphQL API. 

Constraints
-----------

`Constraints <https://www.postgresql.org/docs/9.4/ddl-constraints.html>`_ are used to define rules for fields in a database table. They ensure that
no invalid data is entered into the database.

**Example**

.. code-block:: sql

  CREATE TABLE authors(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    indicator      TEXT    NOT NULL UNIQUE,
    rating         INT     NOT NULL CHECK(rating > 0 AND rating < 11)
  );

There are a number of different constraint types in the above example:

- ``PRIMARY KEY``: There is one ``PRIMARY KEY`` in each table. It's used to identify each specific record stored in the table.
- ``NOT NULL``: Makes sure a certain field is not null.
- ``UNIQUE``: Prevents database entries with a duplicate value of the respective field.
- ``CHECK``: Checks the field value of a database entry for a certain condition.

.. note::

  For more details and examples on Postgres constraints, please refer to the `Postgres documentation <https://www.postgresql.org/docs/9.4/ddl-constraints.html>`_.

Views
-----

A `view <https://www.postgresql.org/docs/9.2/sql-createview.html>`_ is a virtual table in Postgres. It represents the result of a query to one or more underlying tables in Postgres without actually storing data.
Views are used to simplify complex queries since these queries are defined once in the view, and can then be directly queried via the same.

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
^^^^^^^^^^^^^^^^^^

Compared to the regular view described above, `materialized views <https://www.postgresql.org/docs/9.3/rules-materializedviews.html>`_ **do** store data physically in the database.

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

.. note::

  Please refer to the Postgres documentation for more details on `views <https://www.postgresql.org/docs/9.2/sql-createview.html>`_ and `materialized views <https://www.postgresql.org/docs/9.3/rules-materializedviews.html>`_.

Indexes
-------

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

.. _postgres_functions:

Functions
---------

`Postgres functions <https://www.postgresql.org/docs/9.1/sql-createfunction.html>`_ allow you to define a set of operations that can include several statements such as declarations, assignments and conditional workflows.
They are a way of customizing your database schema.

**Example:**

.. code-block:: sql

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

.. note::

  For more information on Postgres functions, please refer to the `Postgres documentation <https://www.postgresql.org/docs/9.1/sql-createfunction.html>`_.


Triggers
--------

`Postgres triggers <https://www.postgresql.org/docs/9.1/sql-createtrigger.html>`_ are used to invoke previously defined Postgres functions *before* or *after* a specific database event (e.g. ``INSERT``) occurs.

**Example:**

Let's say we want the Postgres function :ref:`from above<postgres_functions>` to be executed whenever a new article is about to be inserted or updated.
We can create a trigger as follows:

.. code-block:: plpgsql

  CREATE TRIGGER insert_article BEFORE INSERT OR UPDATE ON "articles" FOR EACH ROW EXECUTE PROCEDURE check_author_active();

If someone now tries to insert an article for an author that is not active, the following error will be thrown:

.. code-block:: plpgsql

  unexpected : Author must be active

.. note::

  For more information on how to create triggers, please refer to the `Postgres documentation <https://www.postgresql.org/docs/9.1/sql-createtrigger.html>`_.
