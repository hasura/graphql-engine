.. meta::
   :description: Use Postgres constraints with Hasura
   :keywords: hasura, docs, postgres, constraints

.. _constraints:

Postgres constraints
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


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
