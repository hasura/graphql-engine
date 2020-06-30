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

**Example: identify the author's id as the primary key**

.. code-block:: sql

  CREATE TABLE authors(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    indicator      TEXT    NOT NULL UNIQUE,
    rating         INT     NOT NULL CHECK(rating > 0 AND rating < 11)
  );

Foreign key constraints
^^^^^^^^^^^^^^^^^^^^^^^

Not-null constraints
^^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^

Unique constraints prevent database entries with a duplicate value of the respective field.

**Example: validate that an author's indicator is unique**

.. code-block:: sql

  CREATE TABLE authors(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    indicator      TEXT    NOT NULL UNIQUE,
    rating         INT     NOT NULL CHECK(rating > 0 AND rating < 11)
  );

Check constraints
^^^^^^^^^^^^^^^^^

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

Postgres constraints & Hasura
-----------------------------

Most Postgres constraints (primary key, foreign key, not-null and unique constraints) can be added to Hasura natively when :ref:`creating tables <create-tables>`.

Postgres check constraints can be used as a form of data validation in Hasura and can be added :ref:`as described here <data_validations_check_constraints>`.
