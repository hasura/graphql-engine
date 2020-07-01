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

`Constraints <https://www.postgresql.org/docs/current/ddl-constraints.html>`__ are used to define rules for fields in a database table. They ensure that
no invalid data is entered into the database.

.. note::

  For more detailed information on Postgres constraints, please refer to the `Postgres documentation <https://www.postgresql.org/docs/current/ddl-constraints.html>`__.

Postgres constraints
--------------------

There are different types of constraints that can be used with Postgres.

Primary key constraints
^^^^^^^^^^^^^^^^^^^^^^^

There is one ``PRIMARY KEY`` in each table. It's used to identify each specific row of a table.

**Example: Identify the author's id as the primary key**

.. code-block:: sql
  :emphasize-lines: 2

  CREATE TABLE authors(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL
  );

Foreign key constraints
^^^^^^^^^^^^^^^^^^^^^^^

A foreign key constraint specifies that the values in a column must match the values appearing in a row of another table. 
Foreign key constraints are used to create relationships between tables.

**Example: Define the author_id in the articles table as a foreign key**

.. code-block:: sql
  :emphasize-lines: 5

  CREATE TABLE articles(
    id        INTEGER PRIMARY KEY,
    title     TEXT,
    author_id INTEGER,
    FOREIGN KEY (author_id) REFERENCES authors (id)
  );

Not-null constraints
^^^^^^^^^^^^^^^^^^^^

A not-null constraint allows you to specify that a column's value cannot be ``null``.

**Example: Validate that an author's name is not null**

.. code-block:: sql
  :emphasize-lines: 2-3

  CREATE TABLE authors(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL
  );

Unique constraints
^^^^^^^^^^^^^^^^^^

Unique constraints prevent database entries with a duplicate value of the respective field.

**Example: Validate that an author's indicator is unique**

.. code-block:: sql
  :emphasize-lines: 4

  CREATE TABLE authors(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    indicator      TEXT    NOT NULL UNIQUE
  );

Check constraints
^^^^^^^^^^^^^^^^^

Check constraints allow you to specify a ``Boolean`` expression for a specific field. 
This Boolean expression must be satisfied (equal to ``true``) by the field value for the object to be inserted.

**Example: Validate that an author's rating is between 0 and 10**

.. code-block:: sql
  :emphasize-lines: 4

  CREATE TABLE authors(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    rating         INT     NOT NULL CHECK(rating > 0 AND rating <= 10)
  );

Postgres constraints & Hasura
-----------------------------

Most Postgres constraints (primary key, foreign key, not-null and unique constraints) can be added to Hasura natively when :ref:`creating tables <create-tables>`.

Postgres check constraints can be used as a form of data validation in Hasura and can be added :ref:`as described here <data_validations_check_constraints>`.
