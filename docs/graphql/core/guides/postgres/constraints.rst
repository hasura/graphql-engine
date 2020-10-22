.. meta::
   :description: Use Postgres constraints with Hasura
   :keywords: hasura, docs, postgres, constraints

.. _postgres_constraints:

Postgres constraints
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

`Postgres constraints <https://www.postgresql.org/docs/current/ddl-constraints.html>`__ are used to define rules for columns in a database table. They ensure that
no invalid data is entered into the database.

.. note::

  For more detailed information on Postgres constraints, please refer to the `Postgres documentation <https://www.postgresql.org/docs/current/ddl-constraints.html>`__.

Postgres constraints
--------------------

There are different types of constraints that can be used with Postgres.

Primary key constraints
^^^^^^^^^^^^^^^^^^^^^^^

A ``PRIMARY KEY`` is used to identify each row of a table uniquely.

**Identify the author's id as the primary key of the authors table:**

.. code-block:: sql
  :emphasize-lines: 2

  CREATE TABLE authors(
    id INT PRIMARY KEY,
    name           TEXT    NOT NULL
  );

Foreign key constraints
^^^^^^^^^^^^^^^^^^^^^^^

A foreign key constraint specifies that the values in a column must match the values appearing in a row of another table. 
Foreign key constraints are used to create relationships between tables.

**Define the author_id in the articles table as a foreign key to the id column in the authors table:**

.. code-block:: sql
  :emphasize-lines: 11

  CREATE TABLE authors(
    id SERIAL PRIMARY KEY,
    name           TEXT    NOT NULL,
    email          TEXT    UNIQUE
  );

  CREATE TABLE articles(
    id SERIAL PRIMARY KEY,
    title          TEXT    NOT NULL,
    author_id INTEGER,
    FOREIGN KEY (author_id) REFERENCES authors (id)
  );

Not-null constraints
^^^^^^^^^^^^^^^^^^^^

A not-null constraint allows you to specify that a column's value cannot be ``null``.

**Validate that an author's name cannot be null:**

.. code-block:: sql
  :emphasize-lines: 2-3

  CREATE TABLE authors(
    id SERIAL PRIMARY KEY,
    name           TEXT    NOT NULL
  );

Unique constraints
^^^^^^^^^^^^^^^^^^

Unique constraints prevent database entries with a duplicate value of the respective column.

**Validate that an author's email is unique:**

.. code-block:: sql
  :emphasize-lines: 4

  CREATE TABLE authors(
    id SERIAL PRIMARY KEY,
    name           TEXT    NOT NULL,
    email          TEXT    UNIQUE
  );

Check constraints
^^^^^^^^^^^^^^^^^

Check constraints allow you to specify a ``Boolean`` expression for one or several columns. 
This Boolean expression must be satisfied (equal to ``true``) by the column value for the object to be inserted.

**Validate that an author's rating is between 1 and 10:**

.. code-block:: sql
  :emphasize-lines: 4

  CREATE TABLE authors(
    id SERIAL PRIMARY KEY,
    name           TEXT    NOT NULL,
    rating         INT     NOT NULL CHECK(rating > 0 AND rating <= 10)
  );

Postgres constraints & Hasura
-----------------------------

Most Postgres constraints (primary key, foreign key, not-null and unique constraints) can be added to Hasura natively when :ref:`creating tables <create_tables>`.

Postgres check constraints can be used as a form of data validation in Hasura and can be added :ref:`as described here <data_validations_check_constraints>`.
