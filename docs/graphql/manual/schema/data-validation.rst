.. meta::
   :description: Data validation in Hasura
   :keywords: hasura, docs, schema, data validation

.. _data_validation:

Data validation
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Sometimes, we need to perform validation in the context of a database event like ``insert``, ``update`` or ``delete``.

The best solution to implement validation depends on the complexity of the use case. 

Simple checks: Postgres check constraints
-----------------------------------------

If the validation concerns the table that is about to be manipulated, we can use `Postgres check constraints <https://www.postgresql.org/docs/9.4/ddl-constraints.html>`__.

**Example:** Check that the ``rating`` for a user is between 1 and 10 when a user is inserted or updated.

In the ``Data -> SQL`` tab on the Hasura console, we can add a Postgres check constraint when creating a table:

.. code-block:: plpgsql

  CREATE TABLE authors (
    id integer,
    name text,
    is_active BOOLEAN,
    rating integer CHECK (rating > 0 AND rating < 11)
  );

If the table already exists, we can add a Postgres check constraint as follows:

.. code-block:: plpgsql

  ALTER TABLE authors ADD CONSTRAINT authors_rating_check CHECK (
   rating > 0 AND rating < 11
  );

If someone now tries to add an author with a rating of ``12``, the following error is thrown:

``Check constraint violation. new row for relation "authors" violates check constraint "authors_rating_check"``.

Validation across tables: Postgres triggers
-------------------------------------------

If the validation involves checks across tables, we can use Postgres triggers.

**Example:** Validate that an article can be added only for an author where ``is_active`` is true.

In the ``Data -> SQL`` tab on the Hasura console, we create a `Postgres function <https://www.postgresql.org/docs/9.1/sql-createfunction.html>`__ to perform the validation. 

.. code-block:: plpgsql

  CREATE FUNCTION check_author_active()
  RETURNS trigger AS $BODY$
  DECLARE active_author BOOLEAN;
  BEGIN
  SELECT author.is_active INTO active_author FROM "authors" author WHERE author.id = NEW."author_id";
  IF active_author != 't' THEN
      RAISE EXCEPTION 'Author must be active';
  END IF;
  RETURN NEW;
  END;
  $BODY$ LANGUAGE plpgsql;

Then we add a `Postgres trigger <https://www.postgresql.org/docs/9.1/sql-createtrigger.html>`__ that is called every time an article is inserted or updated.

.. code-block:: plpgsql

  CREATE TRIGGER insert_article BEFORE INSERT OR UPDATE ON "articles" FOR EACH ROW EXECUTE PROCEDURE check_author_active();

If someone now tries to insert an article for an author where ``is_active`` is ``false``, the following error is thrown:

``Insert failed! unexpected : Author must be active``

.. note::

  If you run the above SQL statements from the SQL tab in the Hasura console, make sure that the ``Track this`` box is **not** checked, since these statements should go into Postgres directly and should not be tracked by Hasura. 

Complex data validation: actions
--------------------------------

If the validation can't be captured with SQL and / or includes a third party service, we recommend using :ref:`Hasura actions <actions>`. 

**Example:** Make sure an author is logged in before creating an article.

An example of an action handler implementation for user login can be found :ref:`here <action_handlers>`.
