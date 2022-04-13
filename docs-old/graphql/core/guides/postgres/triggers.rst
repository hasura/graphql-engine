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

Examples
--------

**Trigger a Postgres function before an article is inserted or updated:**

Let's say we want to check if an author is active before a corresponding article can be inserted or updated. 
We can do so with the following Postgres function:

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

Now we want to have this function executed whenever a new article is about to be inserted or updated.
We can create a Postgres trigger as follows:

.. code-block:: plpgsql

  CREATE TRIGGER insert_article BEFORE INSERT OR UPDATE ON "articles" FOR EACH ROW EXECUTE PROCEDURE check_author_active();

If someone now tries to insert an article for an author that is not active, the following error will be thrown:

.. code-block:: plpgsql

  unexpected : Author must be active

**Refresh a materialized view when an author gets inserted:**

Let's say we want to refresh a materialized view whenever a new author is inserted. 

The following Postgres function refreshes a materialized view:

.. code-block:: plpgsql

  CREATE FUNCTION refresh_materialized_view()
    RETURNS trigger AS $BODY$
    BEGIN
    REFRESH MATERIALIZED VIEW popular_active_authors;
    RETURN NULL;
    END;
    $BODY$ LANGUAGE plpgsql;

Now, to make sure this function gets called whenever a new author is inserted, we can create the following Postgres trigger:

.. code-block:: plpgsql

  CREATE TRIGGER update_materialized_view AFTER INSERT ON "authors" FOR EACH ROW EXECUTE PROCEDURE refresh_materialized_view();

Postgres triggers & Hasura
--------------------------

Postgres triggers can be used to perform business logic such as data validation and can be added :ref:`as described here <pg_data_validations_pg_triggers>`.

.. note::

  Hasura also has :ref:`event triggers <event_triggers>` that can be used to invoke external HTTP APIs for executing custom business logic on
  database events.
