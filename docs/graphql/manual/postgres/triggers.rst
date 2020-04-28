.. meta::
   :description: Use Postgres triggers with Hasura
   :keywords: hasura, docs, postgres, triggers

.. _triggers:

Postgres triggers
=================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


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
