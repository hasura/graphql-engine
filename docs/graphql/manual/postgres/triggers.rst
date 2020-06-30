.. meta::
   :description: Use Postgres triggers with Hasura
   :keywords: hasura, docs, postgres, triggers

.. _postgres_triggers:

Postgres triggers
=================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

`Postgres triggers <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__ are used to invoke previously defined Postgres functions *before* or *after* a specific database event (e.g. ``INSERT``) occurs.

.. note::

  For more information on Postgres triggers, please refer to the `Postgres documentation <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__.

Use cases
---------

**Example:**

Let's say we want the :ref:`Postgres function<postgres_functions>` to be executed whenever a new article is about to be inserted or updated.
We can create a trigger as follows:

.. code-block:: plpgsql

  CREATE TRIGGER insert_article BEFORE INSERT OR UPDATE ON "articles" FOR EACH ROW EXECUTE PROCEDURE check_author_active();

If someone now tries to insert an article for an author that is not active, the following error will be thrown:

.. code-block:: plpgsql

  unexpected : Author must be active

Postgres triggers & Hasura
--------------------------

Postgres triggers can be used as a form of data validation in Hasura and can be added :ref:`as described here <data_validations_pg_triggers>`.
