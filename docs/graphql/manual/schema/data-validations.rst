.. meta::
   :description: Data validations in Hasura
   :keywords: hasura, docs, schema, data validation

.. _data_validations:

Data validations
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Many times, we need to perform validations of input data, say when inserting or updating objects.

The best solution to implement validations depends on the complexity of the use case and the layer where you want to perform these validations. 

Checking with Postgres
----------------------

Simple validations via check constraints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the validation can be embedded in the table definition (i.e. DDL) itself, we can use `Postgres check constraints <https://www.postgresql.org/docs/9.4/ddl-constraints.html>`__.

**Example:** Check that the ``rating`` for a user is between 1 and 10 when a user is inserted or updated.

In the ``Data`` tab on the Hasura console, let's say we created a table as follows:

.. code-block:: plpgsql

  CREATE TABLE authors (
    id integer,
    name text,
    rating integer
  );

Then we can head to the ``Modify`` tab in the table page and add a check constraint in the ``Check Constraints`` section:

.. thumbnail:: ../../../img/graphql/manual/schema/add-check-constraint.png
   :alt: Add check constraint

We can also do this while creating the table from the console.

If someone now tries to add an author with a rating of ``11``, the following error is thrown:

``Check constraint violation. new row for relation "authors" violates check constraint "authors_rating_check"``.

Complex checks via triggers
^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the validation is more complex and requires use of data from other tables or complex functions, then we can use Postgres triggers.

**Example:** Validate that an article does not contain bad words.

Suppose we have 2 tables, one for articles and one for bad words:

.. code-block:: plpgsql

  CREATE TABLE articles(
    article_id serial primary key,
    content text
  );

  CREATE TABLE bad_words(
    id serial primary key,
    word text
  );

In the ``Data -> SQL`` tab on the Hasura console, we can create a `Postgres function <https://www.postgresql.org/docs/9.1/sql-createfunction.html>`__ to check if an article is "clean" before it gets inserted. 

.. code-block:: plpgsql

  CREATE FUNCTION check_article_clean()
  RETURNS trigger AS $$
  DECLARE
    dirtyness real;
  BEGIN
    SELECT similarity(OLD.content, bad_words) into dirtyness;
    IF dirtyness >= 0.25 THEN
      RAISE EXCEPTION 'article is very dirty';
    END IF;
    RETURN OLD;
  END;
  $$ LANGUAGE plpgsql;

Then we add a `Postgres trigger <https://www.postgresql.org/docs/9.1/sql-createtrigger.html>`__ that is called every time an article is inserted or updated.

.. code-block:: plpgsql

  CREATE TRIGGER insert_article BEFORE INSERT OR UPDATE ON "articles" FOR EACH ROW EXECUTE PROCEDURE check_article_clean();

``Insert failed! unexpected : article is very dirty``

.. note::

  If you create the above trigger function from the SQL tab in the Hasura console, make sure that the ``Track this`` box is **not** checked, since trigger functions cannot be be tracked by Hasura. 


Checking with Hasura
--------------------

Using permissions
^^^^^^^^^^^^^^^^^

**Example:** Validate that an article can be added only for an author where ``is_active`` is true.


Using Actions
^^^^^^^^^^^^^

If the validation can't be captured with SQL and / or includes a third party service, we recommend using :ref:`Hasura actions <actions>`. 

**Example:** Make sure an author is logged in before creating an article.

An example of an action handler implementation for user login can be found :ref:`here <action_handlers>`.
