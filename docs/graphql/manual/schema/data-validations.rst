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

**Example:** Check that the ``rating`` for an author is between 1 and 10 only.

Let's say we have a table:

.. code-block:: sql

   author (id uuid, name text, rating integer)

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    Now, we can head to the ``Modify`` tab in the table page and add a check
    constraint in the ``Check Constraints`` section:

    .. thumbnail:: /img/graphql/manual/schema/validation-add-check-constraint.png
      :alt: Add check constraint

  .. tab:: Via CLI

    You can :ref:`create a migration manually <manual_migrations>` with the following statement:

    .. code-block:: sql

      ALTER TABLE author
      ADD CONSTRAINT authors_rating_check CHECK (rating > 0 AND rating <= 10);

    Then apply the migration by running:

    .. code-block:: bash

      hasura migrate apply

  .. tab:: Via API

    You can add a check constraint by making an API call to the :ref:`run_sql API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "ALTER TABLE author ADD CONSTRAINT authors_rating_check CHECK (rating > 0 AND rating <= 10);"
        }
      }

If someone now tries to add an author with a rating of ``11``, the following
error is thrown:


.. graphiql::
  :view_only:
  :query:
    mutation {
      insert_author(
        objects: {
          name: "Enid Blyton",
          rating: 11
        }) {
          affected_rows
        }
    }
  :response:
    {
      "errors": [
        {
          "message": "Check constraint violation. new row for relation \"author\" violates check constraint \"authors_rating_check\"",
          "extensions": {
            "path": "$.selectionSet.insert_author.args.objects",
            "code": "permission-error"
          }
        }
      ]
    }

Learn more about `Postgres check constraints <https://www.postgresql.org/docs/current/ddl-constraints.html>`__.

Using Postgres triggers
-----------------------

If the validation logic is more complex and requires the use of data from other tables
and/or functions, then you can use `Postgres triggers <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__.

**Example:** Validate that an article's ``content`` does not exceed a certain number of words.

Suppose we have the following table:

.. code-block:: sql

  article (id uuid, title text, content text)

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    Now, we can head to the ``Data -> SQL`` tab in the console and
    create a `Postgres function <https://www.postgresql.org/docs/current/sql-createfunction.html>`__
    that checks if an article's content exceeds a certain number of words,
    and then add a `Postgres trigger <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__
    that will call this function every time before an article is inserted or updated.

  .. tab:: Via CLI

    You can :ref:`create a migration manually <manual_migrations>` and add your `Postgres function <https://www.postgresql.org/docs/current/sql-createfunction.html>`__
    and your `Postgres trigger <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__ to it.

    Then apply the migration by running:

    .. code-block:: bash

      hasura migrate apply

  .. tab:: Via API

    You can add a function by making an API call to the :ref:`run_sql API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "<SQL statement>"
        }
      }

.. code-block:: plpgsql

   CREATE FUNCTION check_content_length()
   RETURNS trigger AS $$
   DECLARE content_length INTEGER;
   BEGIN
     -- split article content into words and get count
     select array_length(regexp_split_to_array(NEW.content, '\s'),1) INTO content_length;

     -- throw an error if article content is too long
     IF content_length > 100 THEN
         RAISE EXCEPTION 'Content can not have more than 100 words';
     END IF;

     -- return the article row if no error
     RETURN NEW;
   END;
   $$ LANGUAGE plpgsql;

   CREATE TRIGGER check_content_length_trigger
     BEFORE INSERT OR UPDATE ON "article"
     FOR EACH ROW
     EXECUTE PROCEDURE check_content_length();


Now, if we try to insert an article whose content has more than 100 words, we'll receive
the following error:

.. graphiql::
  :view_only:
  :query:
    mutation {
      insert_article(
        objects: {
          title: "lorem ipsum"
          content: "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean et nisl dolor. Nulla eleifend odio et velit aliquet, sed convallis quam bibendum. Cras consequat elit quis est vehicula, nec dignissim dolor cursus. Phasellus suscipit magna ac turpis pulvinar ultricies. Nulla sed lacus sed metus egestas scelerisque nec sed urna. Fusce lorem velit, efficitur sed luctus in, fringilla ac urna. Maecenas fermentum augue sit amet malesuada imperdiet. Suspendisse mattis dignissim quam, at tempor dui tincidunt sed. Maecenas placerat erat nec erat aliquet rutrum. Mauris congue velit nec ultrices dapibus. Duis aliquam, est ac ultricies viverra, ante augue dignissim massa, quis iaculis ex dui in ex. Curabitur pharetra neque ac nisl fringilla, vel pellentesque orci molestie.",
        }
      ) {
        affected_rows
      }
    }
  :response:
    {
      "errors": [
        {
          "message": "postgres query error",
          "extensions": {
            "internal": {
              "error": {
                "exec_status": "FatalError",
                "message": "Content can not have more than 100 words",
                "status_code": "P0001",
              },
            },
            "path": "$.selectionSet.insert_article.args.objects",
            "code": "unexpected"
          }
        }
      ]
    }

Learn more about `Postgres triggers <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__.

Using Hasura permissions
------------------------

If the validation logic can be expressed **declaratively** using static values and
data from the database, then you can use :ref:`row level permissions <row-level-permissions>`
to perform the validations. (Read more about :ref:`Authorization <authorization>`).

**Example 1:** Validate that an ``article`` can be inserted only if ``title`` is not empty.

Suppose, we have a table:

.. code-block:: sql

  article (id uuid, title text, content text, author_id uuid)

Now, we can create a role ``user`` and add the following rule:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. thumbnail:: /img/graphql/manual/schema/validation-not-empty.png
      :alt: validation using permission: title cannot be empty

  .. tab:: Via CLI
  
    You can add roles and permissions in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
      :emphasize-lines: 4-9

        - table:
            schema: public
            name: article
          insert_permissions:
          - role: user
            permission:
              check:
                title:
                  _ne: ''

    After that, apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: Via API

    You can add column presets by making an API call to the :ref:`create_insert_permission API <create_insert_permission>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "create_insert_permission",
        "args": {
          "table": "article",
          "role": "user",
          "permission": {
            "check": {
              "title": {
                "_ne": ""
              }
            }
          }
        }
      }

If we try to insert an article with ``title = ""``, we will get a ``permission-error``:

.. graphiql::
  :view_only:
  :query:
    mutation {
      insert_article(
        objects: {
          title: ""
          content: "Lorem ipsum dolor sit amet",
        }
      ) {
        affected_rows
      }
    }
  :response:
    {
      "errors": [
        {
          "message": "Check constraint violation. insert check constraint failed",
          "extensions": {
            "path": "$.selectionSet.insert_article.args.objects",
            "code": "permission-error"
          }
        }
      ]
    }

**Example 2:**  Validate that an ``article`` can be inserted only if its ``author`` is active.

Suppose, we have 2 tables:

.. code-block:: sql

  author (id uuid, name text, is_active boolean)
  article (id uuid, author_id uuid, content text)

Also, suppose there is an :ref:`object relationship <graphql_relationships>` ``article.author`` defined as:

.. code-block:: sql

  article.author_id -> author.id

Now, we can create a role ``user`` and add the following rule:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. thumbnail:: /img/graphql/manual/schema/validation-author-isactive.png
      :alt: validation using permissions: author should be active

  .. tab:: Via CLI

    You can add roles and permissions in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
      :emphasize-lines: 4-10

        - table:
            schema: public
            name: article
          insert_permissions:
          - role: user
            permission:
              check:
                author:
                  is_active:
                    _eq: true

    After that, apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: Via API

    You can add column presets by making an API call to the :ref:`create_insert_permission API <create_insert_permission>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "create_insert_permission",
        "args": {
          "table": "article",
          "role": "user",
          "permission": {
            "check": {
              "author": {
                "is_active": true
              }
            }
          }
        }
      }

If we try to insert an article for an author for whom ``is_active = false``, we
will receive a ``permission-error`` :

.. graphiql::
  :view_only:
  :query:
    mutation {
      insert_article(
        objects: {
          title: "lorem ipsum"
          content: "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
          author_id: 2
        }
      ) {
        affected_rows
      }
    }
  :response:
    {
      "errors": [
        {
          "message": "Check constraint violation. insert check constraint failed",
          "extensions": {
            "path": "$.selectionSet.insert_article.args.objects",
            "code": "permission-error"
          }
        }
      ]
    }


.. note::

  Permissions are scoped to a user's role. So, if a validation check
  needs to be global then you will have to define it for all roles which have
  insert/update permissions.

  A few features on the roadmap should simplify this experience in the future.

Using Hasura Actions
--------------------

If the validation requires complex custom business logic and/or needs information
from external sources, you can use :ref:`Actions <actions>` to perform your
validation.

**Example:** Check with an external service that an author's name is not black-listed
before inserting them.

Let's assume we have an external service that stores and manages black-listed authors.
Before inserting an author we need to check with this service if they are black-listed
or not.

The validation process looks as follows:

.. thumbnail:: /img/graphql/manual/schema/actions-data-validation.png
   :alt: validation using actions: article not blacklisted
   :width: 60%


Actions allow us to define :ref:`custom types <custom_types>` in our GraphQL schema.

We create a new action called ``InsertAuthor`` that takes an ``author`` object with type ``AuthorInput`` as input and
returns an object of type ``AuthorOutput``:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. thumbnail:: /img/graphql/manual/schema/validation-actions-def.png
      :alt: Create action

  .. tab:: Via CLI

    An action can be created by adding it to the ``actions.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml

        actions:
          - name: InsertAuthor
            definition:
              kind: synchronous
              handler: http://host.docker.internal:3000
          custom_types:
            enums: []
            input_objects:
            - name: AuthorInput
            objects:
            - name: AddResult
            - name: UpdatedAuthor
            - name: AuthorOutput
            scalars: []

    Then apply the metadata by running:

    .. code-block:: bash

        hasura metadata apply

  .. tab:: Via API

    It is essential that the custom types used in the action are defined *beforehand* via the :ref:`set_custom_types metadata API <set_custom_types>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "set_custom_types",
        "args": {
          "scalars": [],
          "enums": [],
          "input_objects": [
            {
              "name": "AuthorInput",
              "fields": [
                {
                  "name": "name",
                  "type": "String!"
                },
                {
                  "name": "rating",
                  "type": "Int!"
                },
                {
                  "name": "is_active",
                  "type": "Boolean!"
                }
              ]
            }
          ],
          "objects": [
            {
              "name": "AuthorOutput",
              "fields": [
                {
                  "name": "id",
                  "type": "Int!"
                }
              ]
            }
          ]
        }
      }

    Once the custom types are defined, we can create an action via the :ref:`create_action metadata API <create_actions>`:

    .. code-block:: http

        POST /v1/query HTTP/1.1
        Content-Type: application/json
        X-Hasura-Role: admin

        {
          "type": "create_action",
          "args": {
            "name": "InsertAuthor",
            "type": "mutation",
            "definition": {
                "kind": "synchronous",
                "arguments": [
                  {
                    "name": "name",
                    "type": "String!"
                  },
                  {
                    "name": "rating",
                    "type": "Int!"
                  },
                  {
                    "name": "is_active",
                    "type": "Boolean!"
                  }
                ],
                "output_type": "AuthorOutput",
                "handler": "https://action.my_app.com/create-user"
              }
          }
        }
 

The business logic of an action - in our case the author validation - happens in the :ref:`action handler <action_handlers>`
which is an HTTP webhook which contains the code to call the external service.

The following is a sample code that could be added to the event handler to implement the data validation:

.. code-block:: javascript

  function getBlacklistedAuthorsFromApi() {
    // make external api call & return black-listed authors list
  }

  function insertAuthorViaHasura() {
    // run insert_author mutation & return response
  }

  const blacklistedAuthors = getBlacklistedAuthorsFromApi();

  if (blacklistedAuthors.includes(author.name)) {
    return res.status(400).json({ message: "Author is blacklisted" });
  } else {
    const insertAuthorResponse = insertAuthorViaHasura();

    return res.json(insertAuthorResponse);
  }

When we now insert an author, our action handler will be called and it will check if the author is black-listed.
If it's not, the author will be inserted and the ``id`` will be returned. If the author is black-listed,
we get the following error message:

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

  For actual examples of data validations with actions, refer to the `actions examples repo <https://github.com/hasura/hasura-actions-examples/tree/master/data-validations>`__.
