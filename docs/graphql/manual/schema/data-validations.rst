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

This is ideal if you want the validations to be part of your Postgres DDL.

Simple validations via check constraints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the validation can be embedded in the table definition,
we can use `Postgres check constraints <https://www.postgresql.org/docs/9.4/ddl-constraints.html>`__.

**Example:** Check that the ``rating`` for an author is between 1 and 10 only.

Let's say we have a table:

.. code-block:: sql

   author(id uuid, name text, rating integer)

Now, we can head to the ``Modify`` tab in the table page and add a check constraint in the ``Check Constraints`` section
(this is available in ``Create Table`` page as well) :

.. thumbnail:: ../../../img/graphql/manual/schema/add-check-constraint.png
   :alt: Add check constraint

If someone now tries to add an author with a rating of ``11``, the following error is thrown:

.. code-block:: text

  Check constraint violation. new row for relation "authors" violates check constraint "authors_rating_check"

Learn more about `Postgres check constraints <https://www.postgresql.org/docs/9.4/ddl-constraints.html>`__.

Complex checks via triggers
^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the validation is more complex and requires use of data from many tables or complex functions,
then we can use Postgres triggers.

**Example:** Validate that an article does not contain bad words.

Suppose we have 2 tables:

.. code-block:: sql

  articles(article_id uuid, content text)
  bad_words(word text)

We can create a `Postgres function <https://www.postgresql.org/docs/9.1/sql-createfunction.html>`__ that checks if an article is "clean". For this, we will use the ``similarity`` function (read more `here <https://www.postgresql.org/docs/9.6/pgtrgm.html>`__). In the ``Data -> SQL`` tab on the Hasura console, run the following SQL:

.. code-block:: plpgsql

  CREATE FUNCTION check_article_clean()
   RETURNS trigger
   LANGUAGE plpgsql
  AS $$
  DECLARE
    all_bad_words text;
    dirtyness real;
  BEGIN
    SELECT string_agg(name, ' ') INTO all_bad_words from bad_words;
    SELECT similarity(NEW.content, all_bad_words) INTO dirtyness;
    IF dirtyness >= 0.25 THEN
      RAISE EXCEPTION 'article is very dirty';
    END IF;
    RETURN NEW;
  END;
  $$

We also need to add a `Postgres trigger <https://www.postgresql.org/docs/9.1/sql-createtrigger.html>`__ that will be called every time an article is inserted or updated.

.. code-block:: plpgsql

  CREATE TRIGGER insert_article BEFORE INSERT OR UPDATE ON "articles" FOR EACH ROW EXECUTE PROCEDURE check_article_clean();

Now, if we try to insert an article which has lot of bad words, we would recieve an error:

``Insert failed! unexpected : article is very dirty``

.. note::

  If you create the above trigger function from the SQL tab in the Hasura console, make sure that the ``Track this`` box is **not** checked since trigger functions are note trackable. 


Using Hasura
------------

Declarative validations via permissions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the validations can be expressed **declaratively**, then we can use the permission rules in Hasura Auth to perform the validations.

**Example 1:** Validate that an inventory can only have ``stock >= 0`` for any item.

Suppose, we have a table:

.. code-block:: sql

  inventory (item_id uuid, item_name text, stock integer)

Now, we can create a role ``user`` on this table with the following rule:

.. thumbnail:: ../../../img/graphql/manual/schema/validation-stock-gte-zero.png
   :alt: validation using permission: stock should be greater than or equal to zero

If we try to insert an item with ``stock = -1``, we will get a ``permission-error``:

``Check constraint violation. insert check constraint failed``

**Example 2:**  Validate that an article can be added for an author only if ``is_active = true``.

Suppose, we have 2 tables:

.. code-block:: sql

  author (id uuid, name text, is_active boolean)
  articles(id uuid, author_id uuid, content text)

Also, suppose there is an object relationship ``articles.author`` defined as:

.. code-block:: sql

  articles.id -> author.id

Now, we can create a role ``user`` on this table with the following rule:

.. thumbnail:: ../../../img/graphql/manual/schema/validation-author-isactive.png
   :alt: validation using permissions: author should be active

Similar to previous example, if we try to insert an article for an author for whom ``is_active = false``, we will receive a ``permission-error`` response.

.. note::

  Permissions are scoped to a role. So, if a validation check needs to be global then you will have to define it for all roles. We have few features in the roadmap which will simplify this in the future.

Custom logic via Actions
^^^^^^^^^^^^^^^^^^^^^^^^

If the validations are not declarative and/or require custom business logic, we recommend using :ref:`Hasura Actions <actions>`. 

**Example:** Make sure an author is not black-listed when creating an article.

<TODO>
