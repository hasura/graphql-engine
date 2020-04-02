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

Using Postgres
--------------

This is ideal if you want the validations to be part of the table definition at Postgres itself.

**Example 1:** Check that the ``rating`` for a user is between 1 and 10 when a user is inserted or updated.

Let's say we have a table ``author`` with a integer column ``rating``.

Now, we can head to the ``Modify`` tab in the table page and add a check constraint in the ``Check Constraints`` section
(this is available in ``Create Table`` page as well) :

.. thumbnail:: ../../../img/graphql/manual/schema/add-check-constraint.png
   :alt: Add check constraint



If someone now tries to add an author with a rating of ``11``, the following error is thrown:

``Check constraint violation. new row for relation "authors" violates check constraint "authors_rating_check"``.

Learn more about `Postgres check constraints <https://www.postgresql.org/docs/9.4/ddl-constraints.html>`__.


**Example 2:** Validate that an article does not contain bad words.

Suppose we have 2 tables ``articles(article_id uuid, context text)`` and ``bad_words(word text)``:

We can create a `Postgres function <https://www.postgresql.org/docs/9.1/sql-createfunction.html>`__ that checks if an article is "clean".  In the ``Data -> SQL`` tab on the Hasura console, run the following SQL:

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

We also need to add a `Postgres trigger <https://www.postgresql.org/docs/9.1/sql-createtrigger.html>`__ that will be called every time an article is inserted or updated.

.. code-block:: plpgsql

  CREATE TRIGGER insert_article BEFORE INSERT OR UPDATE ON "articles" FOR EACH ROW EXECUTE PROCEDURE check_article_clean();

Now, if you insert an article which has lot of bad words, you would recieve an error:

``Insert failed! unexpected : article is very dirty``

.. note::

  If you create the above trigger function from the SQL tab in the Hasura console, make sure that the ``Track this`` box is **not** checked, since trigger functions cannot be be tracked by Hasura. 


Using Hasura
------------

With permissions
^^^^^^^^^^^^^^^^

If the validations are declarative, then permission rules in Hasura Auth can be used.

**Example 1:** Validate that an inventory can only have ``stock >= 0`` for any item.

Suppose, we have a table ``inventory (item_id uuid, item_name text, stock integer)``

Now, suppose you create a role ``user`` on this table with the following rule:

<insert image>

**Example 2:**  Validate that an article can be added for an author only if ``is_active = true``.

Suppose, we have 2 tables:

1. ``author (id uuid, name text, is_active boolean)``
2. ``articles(id uuid, author_id uuid, content text)``.

Also, suppose there is an object relationship ``articles.author`` defined as ``articles.id -> author.id``.

Now, suppose you create a role ``user`` on this table with the following rule:

<insert image>

.. note::

  Permissions are scoped to a role. So, if a validation check needs to be global then you will have to define it for all roles. We have few features in the roadmap which will simplify this in the future.

With Actions
^^^^^^^^^^^^

If the validations are not declarative and/or require custom business logic, we recommend using :ref:`Hasura Actions <actions>`. 

**Example:** Make sure an author is not black-listed when creating an article.

<TODO>
