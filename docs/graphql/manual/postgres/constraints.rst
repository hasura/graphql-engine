.. meta::
   :description: Use Postgres constraints with Hasura
   :keywords: hasura, docs, postgres, constraints

.. _postgres_constraints:

Postgres constraints
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

`Constraints <https://www.postgresql.org/docs/current/ddl-constraints.html>`__ are used to define rules for fields in a database table. They ensure that
no invalid data is entered into the database.

**Example: creat table statement constraints**

.. code-block:: sql

  CREATE TABLE authors(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    indicator      TEXT    NOT NULL UNIQUE,
    rating         INT     NOT NULL CHECK(rating > 0 AND rating <= 10)
  );

Check constraints
-----------------

Check constraints allow you to specify a ``Boolean`` expression for a specific field. 
This Boolean expression must be satisfied (equal to ``true``) by the field value for the object to be inserted.

**Example: validate that an author's rating is between 0 and 10**

.. code-block:: sql

  CREATE TABLE authors(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    indicator      TEXT    NOT NULL UNIQUE,
    rating         INT     NOT NULL CHECK(rating > 0 AND rating <= 10)
  );


Not-null constraints
--------------------

A not-null constraint allows you to specify that a column's value cannot be ``null``.

**Example: validate that an author's name is not null**

.. code-block:: sql

  CREATE TABLE authors(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    indicator      TEXT    NOT NULL UNIQUE,
    rating         INT     NOT NULL CHECK(rating > 0 AND rating < 11)
  );

Unique constraints
------------------

Unique constraints prevent database entries with a duplicate value of the respective field.

**Example: validate that an author's indicator is unique**

.. code-block:: sql

  CREATE TABLE authors(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    indicator      TEXT    NOT NULL UNIQUE,
    rating         INT     NOT NULL CHECK(rating > 0 AND rating < 11)
  );

Primary keys
------------

There is one ``PRIMARY KEY`` in each table. It's used to identify each specific row of a table.

**Example: identify the author's id as the primary key**

.. code-block:: sql

  CREATE TABLE authors(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    indicator      TEXT    NOT NULL UNIQUE,
    rating         INT     NOT NULL CHECK(rating > 0 AND rating < 11)
  );

Foreign keys
------------

Constraints & Hasura
--------------------

.. note::

  For more details and examples on Postgres constraints, please refer to the `Postgres documentation <https://www.postgresql.org/docs/current/ddl-constraints.html>`__.
