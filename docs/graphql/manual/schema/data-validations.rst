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

Overview
--------

Many times, we need to perform validations of input data before inserting or
updating objects.

The best solution to implement a validation depends on the complexity of the
validation logic and the layer where you would like to add it.

- If you would like the validation logic to be a part of your database schema,
  Postgres check constraints or triggers would be ideal solutions to add your
  validation.

- If you would like the validation logic to be at the GraphQL API layer, Hasura
  permissions can be used to add your validation.

- If the validation logic requires complex business logic and/or needs
  information from external sources, you can use Hasura Actions to perform your
  validation.

These solutions are explained in some more detail below.

Using Postgres check constraints
--------------------------------

If the validation logic can be expressed by using only static values and the
columns of the table, you can use `Postgres check constraints <https://www.postgresql.org/docs/current/ddl-constraints.html>`__.

.. min_price > 0. max_price >= min_price, selling_price >= min_price AND selling_price <= max_price

**Example:** Check that the ``rating`` for an author is between 1 and 10 only.

Let's say we have a table:

.. code-block:: sql

   author(id uuid, name text, rating integer)

Now, we can head to the ``Modify`` tab in the table page and add a check
constraint in the ``Check Constraints`` section:

.. thumbnail:: ../../../img/graphql/manual/schema/add-check-constraint.png
   :alt: Add check constraint

If someone now tries to add an author with a rating of ``11``, the following
error is thrown:

.. code-block:: text

  Check constraint violation. new row for relation "authors" violates check
  constraint "authors_rating_check"

Learn more about `Postgres check constraints <https://www.postgresql.org/docs/9.4/ddl-constraints.html>`__.

Using Postgres triggers
-----------------------

If the validation logic is more complex and requires use of data from other tables
and/or functions, then you can use `Postgres triggers <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__.

**Example:** Validate that an article does not contain bad words.

Suppose we have 2 tables:

.. code-block:: sql

  articles(article_id uuid, content text)
  bad_words(word text)

Now, we can head to the ``Data -> SQL`` tab in the console and
create a `Postgres function <https://www.postgresql.org/docs/current/sql-createfunction.html>`__
that checks if an article is "clean" using the PG ``similarity`` function (described `here <https://www.postgresql.org/docs/current/pgtrgm.html#id-1.11.7.40.5>`__)
and then add a `Postgres trigger <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__
that will call this function every time before an article is inserted or updated.

.. code-block:: plpgsql

  CREATE FUNCTION check_article_clean()
   RETURNS trigger
   LANGUAGE plpgsql
  AS $$
  DECLARE
    all_bad_words text;
    dirtyness real;
  BEGIN
    -- get all bad words from the "bad_words" table
    SELECT string_agg(name, ' ') INTO all_bad_words from bad_words;

    -- check the overlap between the bad words and the article content using
    SELECT similarity(NEW.content, all_bad_words) INTO dirtyness;

    -- throw an error if the article is too dirty
    IF dirtyness >= 0.25 THEN
      RAISE EXCEPTION 'article is very dirty';
    END IF;

    -- return the article row if no error
    RETURN NEW;
  END;
  $$

  CREATE TRIGGER insert_article
    BEFORE INSERT OR UPDATE ON "articles"
    FOR EACH ROW
    EXECUTE PROCEDURE check_article_clean();

Now, if we try to insert an article which has lot of bad words, we would receive
an error:

.. code-block:: text

  Insert failed! unexpected : article is very dirty

.. note::

  If you create the trigger function from the SQL tab in the Hasura console,
  make sure that the ``Track this`` box is **not** checked since trigger functions
  are not trackable.

Using Hasura permissions
------------------------

If the validation logic can be expressed **declaratively** using static values and
data from the database, then you can use :ref:`row level permissions <row-level-permissions>`
to perform the validations. (Read more about :ref:`Authorization <authorization>`).

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

Similar to the previous example, if we try to insert an article for an author for
whom ``is_active = false``, we will receive a ``permission-error`` response.

.. note::

  Permissions are scoped to a user role. So, if a validation check needs to be
  global then you will have to define it for all roles which have insert/update
  permissions.

  A few features on the roadmap should simplify this experience in the future.

Using Hasura Actions
--------------------

If the validation requires complex custom business logic and/or needs information
from external sources, you can use :ref:`Actions <actions>` to perform your
validation.

**Example:** Make sure an author is not black-listed before inserting them.

Let's assume you have an external service that manages and stores black-listed authors.
Before inserting an author in our GraphQL API, we need to check with this service if an author is black-listed or not.

Let's assume we have :ref:`derived an action <derive_actions>` from the ``insert_authors_one`` muation. It has the following action definition:

.. code-block:: graphql

  type Mutation {
    InsertAuthor (
      author: AuthorInput!
    ): AuthorOutput
  }

And the following :ref:`type definitions <custom_types>`:

.. code-block:: graphql

  input AuthorInput {
    indicator : String
    is_active : Boolean
    name : String
    popular_author : Boolean
    rating : Int
  }

  type AuthorOutput {
    id : Int!
  }

Add the following business logic to your :ref:`action handler <action_handlers>`:

.. code-block:: javascript

  // run some business logic

  const blacklistedAuthors = ["Dr. Doom", "Thanos", "Joker"];

  if (blacklistedAuthors.includes(author.name)) {
    res.status(400).json({ message: "Author is blacklisted" });
  }

When we now insert an author, our action handler will check if author is black-listed. If it's not, the author will be inserted into our GraphQL API and the ``id`` will be returned.
If the author is black-listed, we get the following error message:

.. code-block:: text

  "Author is blacklisted"

.. note::

  For examples of data validations with actions, please refer to this `Github repo <https://github.com/hasura/hasura-actions-examples/tree/master/data-validations>`__.
