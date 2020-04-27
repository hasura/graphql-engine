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

Learn more about `Postgres check constraints <https://www.postgresql.org/docs/current/ddl-constraints.html>`__.

Using Postgres triggers
-----------------------

If the validation logic is more complex and requires the use of data from other tables
and/or functions, then you can use `Postgres triggers <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__.

**Example:** Validate that an article's content does not exceed a certain number of words.

Suppose we have the following table:

.. code-block:: sql

  articles(article_id uuid, content text)

Now, we can head to the ``Data -> SQL`` tab in the console and
create a `Postgres function <https://www.postgresql.org/docs/current/sql-createfunction.html>`__
that checks if an article's content exceeds a certain number of words,
and then add a `Postgres trigger <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__
that will call this function every time before an article is inserted or updated.

.. code-block:: plpgsql

  CREATE FUNCTION check_content_length()
  RETURNS trigger AS $$
  DECLARE content_length INTEGER;
  BEGIN
    -- count words in article
    SELECT array_length(regexp_split_to_array(NEW.content, '\s'),1) INTO content_length;
   
    -- throw an error if article is too long 
    IF content_length > 100 THEN
      RAISE EXCEPTION 'Content cannott have more than 100 words';
    END IF;
    
    -- return the article row if no error
    RETURN NEW;
  END;
  $$ LANGUAGE plpgsql;
  
  CREATE TRIGGER insert_article_content BEFORE INSERT OR UPDATE ON "articles" FOR EACH ROW EXECUTE PROCEDURE check_content_length();

Now, if we try to insert an article whose content has more than 100 words, we'll receive
the following error:

.. code-block:: text

  Insert failed! unexpected : Content can't have more than 100 words

.. note::

  If you create the trigger function from the SQL tab in the Hasura console,
  make sure that the ``Track this`` box is **not** checked since trigger functions
  are not trackable.

Learn more about `Postgres triggers <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__.

Using Hasura permissions
------------------------

If the validation logic can be expressed **declaratively** using static values and
data from the database, then you can use :ref:`row level permissions <row-level-permissions>`
to perform the validations. (Read more about :ref:`Authorization <authorization>`).

**Example 1:** Validate that an article cannot be inserted if ``title = null``.

Suppose, we have a table:

.. code-block:: sql

  article (id uuid, title text, content text, article_id uuid)

Now, we can create a role ``user`` on this table with the following rule:

.. thumbnail:: ../../../img/graphql/manual/schema/validation-not-null.png
   :alt: validation using permission: title cannot be null

If we try to insert an article with ``title = null``, we will get a ``permission-error``:

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

**Example:** Check with an external service that an author's name is not black-listed before inserting them in the GraphQL API.

Let's assume we have an external service that stores and manages black-listed authors.
Before inserting an author in our GraphQL API, we need to check with this service if an author is black-listed or not.
The validation process looks as follows:

.. thumbnail:: ../../../img/graphql/manual/schema/actions-data-validation.png
   :alt: validation using actions: article not blacklisted
   :width: 60%

We create a new action called ``InsertAuthor`` that takes an ``author`` object with type ``AuthorInput`` as input and 
returns an object of type ``AuthorOutput``:

.. code-block:: graphql

  type Mutation {
    InsertAuthor (
      author: AuthorInput!
    ): AuthorOutput
  }

Actions allow us to define :ref:`custom types <custom_types>`. In the above action, we defined ``AuthorInput`` and ``AuthorOutput`` as new custom types:
We define these two new custom types as follows:

.. code-block:: graphql

  input AuthorInput {
    name : String
    rating : Int
    is_active : Boolean
  }

  type AuthorOutput {
    id : Int!
  }

The business logic of an action - in our case the author validation - happens in the :ref:`action handler <action_handlers>` (HTTP webhook).
It is the *external* service that is called. The following is a sample code that could be added to the event handler to implement the data validation:

.. code-block:: javascript

  // business logic

  const blacklistedAuthors = ["Dr. Doom", "Thanos", "Joker"];

  if (blacklistedAuthors.includes(author.name)) {
    res.status(400).json({ message: "Author is blacklisted" });
  }

When we now insert an author, our action handler will be called and it will check if the author is black-listed. If it's not, the author will be inserted into our GraphQL API and the ``id`` will be returned.
If the author is black-listed, we get the following error message:

.. graphiql::
  :view_only:
  :query:
    mutation insertArticle {
      InsertAuthor(author: { name: "Thanos" }) {
        id
      }
    } 
  :response:
    {
      "errors": [
        {
          "extensions": {
            "path": "$",
            "code": "unexpected"
          },
          "message": "Author is blacklisted"
        }
      ]
    }

.. note::

  For examples of data validations with actions, please refer to this `Github repo <https://github.com/hasura/hasura-actions-examples/tree/master/data-validations>`__.
