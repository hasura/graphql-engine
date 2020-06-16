.. meta::
   :description: Create relationships in Hasura
   :keywords: hasura, docs, schema, relationship, create

.. _create_relationships:

Creating relationships
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

A relationship from one table/view to another can be created by defining a link between a column of the table/view to a
column of the other table/view.

Typically, relationships are defined using foreign-key constraints. But in some cases, it might not be possible to
use foreign-key constraints to create the relation. For example, while trying to create a relationship involving a view
as foreign-keys can't be created on views.

.. _relationships-using-fkey:

Using foreign keys
------------------

Say we created two tables, ``author(id, name)`` and ``article(id, title, content, rating, author_id)``.

Let us now connect these tables to enable nested queries using a foreign-key:

Step 1: Add foreign-key constraint
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's add a foreign-key constraint to the ``author_id`` column in the ``article`` table.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    In the console, navigate to the ``Modify`` tab of the ``article`` table. Click the ``Add`` button in
    the Foreign Keys section and configure the ``author_id`` column as a foreign-key for the ``id`` column in
    the ``author`` table:

    .. thumbnail:: /img/graphql/manual/schema/add-foreign-key.png
      :alt: Add foreign-key constraint

  .. tab:: Via CLI

    :ref:`Create a migration manually <manual_migrations>` and add the following statement to it:

    .. code-block:: sql

      ALTER TABLE article
      ADD FOREIGN KEY (author_id) REFERENCES author(id);

    Apply the migration by running:

    .. code-block:: bash

      hasura migrate apply

  .. tab:: Via API

    You can add a foreign-key constraint using the :ref:`run_sql metadata API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "ALTER TABLE article ADD FOREIGN KEY (author_id) REFERENCES author(id);"
        }
      }

Step 2: Create an object relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Each article has one author. This is an ``object relationship``.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    The console infers potential relationships using the foreign-key created above and recommends these in the
    ``Relationships`` tab of the ``article`` table.

    Add an ``object relationship`` named ``author`` for the ``article`` table as shown here:

    .. thumbnail:: /img/graphql/manual/schema/add-1-1-relationship.png
      :alt: Create an object relationship

  .. tab:: Via CLI

    You can add an object relationship in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
       :emphasize-lines: 4-7

        - table:
            schema: public
            name: article
          object_relationships:
          - name: author
            using:
              foreign_key_constraint_on: author_id
        - table:
            schema: public
            name: author

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: Via API

    You can create an object relationship by using the :ref:`create_object_relationship metadata API <create_object_relationship>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "create_object_relationship",
        "args": {
          "table": "article",
          "name": "author",
          "using": {
            "foreign_key_constraint_on": "author_id"
          }
        }
      }

We can now run a nested object query that is based on this ``object relationship``.

Fetch a list of articles and each article's author:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: GraphiQL

    .. graphiql::
      :view_only:
      :query:
        query {
          article {
            id
            title
            author {
              id
              name
            }
          }
        }
      :response:
        {
          "data": {
            "article": [
              {
                "id": 1,
                "title": "sit amet",
                "author": {
                  "name": "Anjela",
                  "id": 4
                }
              },
              {
                "id": 2,
                "title": "a nibh",
                "author": {
                  "name": "Beltran",
                  "id": 2
                }
              },
              {
                "id": 3,
                "title": "amet justo morbi",
                "author": {
                  "name": "Anjela",
                  "id": 4
                }
              }
            ]
          }
        }

  .. tab:: API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { article { id title author { id name }}}"
      }

Step 3: Create an array relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An author can write multiple articles. This is an ``array relationship``.

You can add an ``array relationship`` in the same fashion as an ``object relationship`` as shown above.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    On the console, add an ``array relationship`` named ``articles`` for the ``author`` table as shown here:

    .. thumbnail:: /img/graphql/manual/schema/add-1-many-relationship.png
      :alt: Create an array relationship

    We can now run a nested object query that is based on this ``array relationship``.

  .. tab:: Via CLI

    You can add an array relationship in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
      :emphasize-lines: 11-18

        - table:
            schema: public
            name: article
          object_relationships:
          - name: author
            using:
              foreign_key_constraint_on: author_id
        - table:
            schema: public
            name: author
          array_relationships:
          - name: articles
            using:
              foreign_key_constraint_on:
                column: author_id
                table:
                  schema: public
                  name: article

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: Via API

    You can create an array relationship by using the :ref:`create_array_relationship metadata API <create_array_relationship>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "create_array_relationship",
        "args": {
          "table": "author",
          "name": "articles",
          "using": {
            "foreign_key_constraint_on" : {
              "table" : "article",
              "column" : "author_id"
            }
          }
        }
      }

Fetch a list of authors and a nested list of each author's articles:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: GraphiQL

    Make the following GraphQL query on the Hasura console:

    .. graphiql::
      :view_only:
      :query:
        query {
          author {
            id
            name
            articles {
              id
              title
            }
          }
        }
      :response:
        {
          "data": {
            "author": [
              {
                "id": 1,
                "name": "Justin",
                "articles": [
                  {
                    "id": 15,
                    "title": "vel dapibus at"
                  },
                  {
                    "id": 16,
                    "title": "sem duis aliquam"
                  }
                ]
              },
              {
                "id": 2,
                "name": "Beltran",
                "articles": [
                  {
                    "id": 2,
                    "title": "a nibh"
                  },
                  {
                    "id": 9,
                    "title": "sit amet"
                  }
                ]
              },
              {
                "id": 3,
                "name": "Sidney",
                "articles": [
                  {
                    "id": 6,
                    "title": "sapien ut"
                  },
                  {
                    "id": 11,
                    "title": "turpis eget"
                  },
                  {
                    "id": 14,
                    "title": "congue etiam justo"
                  }
                ]
              }
            ]
          }
        }

  .. tab:: API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { author { id name articles { id title }}}"
      }

.. _create_manual_relationships:

Using manual relationships
--------------------------

Let's say you have a table ``author (id, name)`` and a view ``author_avg_rating (id, avg)`` which has the
average rating of articles for each author.

Let us now create an ``object relationship`` called ``avg_rating`` from the ``author`` table to the
``author_avg_rating`` view using a manual relationship:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    Step 1: Open the manual relationship section
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    - Open the console and navigate to the ``Data -> author -> Relationships`` tab.
    - Click on the ``Configure`` button:

    .. thumbnail:: /img/graphql/manual/schema/manual-relationship-btn.png
      :alt: Open the manual relationship section

    Step 2: Define the relationship
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The above step will open up a section as shown below:

    .. thumbnail:: /img/graphql/manual/schema/manual-relationship-create.png
      :alt: Define the relationship

    In this case:

    - **Relationship Type** will be: ``Object Relationship``
    - **Relationship Name** can be: ``avg_rating``
    - **Reference** will be: ``id -> author_avg_rating . id`` *(similar to defining a foreign-key)*

    Step 3: Create the relationship
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Now click on the ``Save`` button to create the relationship.

  .. tab:: Via CLI

    You can add a manual relationship in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
       :emphasize-lines: 7-15

        - table:
            schema: public
            name: article
        - table:
            schema: public
            name: author
          object_relationships:
          - name: avg_rating
            using:
              manual_configuration:
                remote_table:
                  schema: public
                  name: author_average_rating
                column_mapping:
                  id: author_id
        - table:
            schema: public
            name: author_average_rating

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: Via API

    You can add a manual relationship by using the :ref:`create_object_relationship metadata API <create_object_relationship>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "create_object_relationship",
        "args": {
          "table": "author",
          "name": "avg_rating",
          "using": {
            "manual_configuration": {
              "remote_table": "author_average_rating",
              "column_mapping": {
                "id": "author_id"
              }
            }
          }
        }
      }

We can now run a nested object query that is based on this ``object relationship``.

Fetch a list of authors with the average rating of their articles:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: GraphiQL

    .. graphiql::
      :view_only:
      :query:
        query {
          author {
            id
            name
            avg_rating {
              avg
            }
          }
        }
      :response:
        {
          "data": {
            "author": [
              {
                "id": 1,
                "name": "Justin",
                "avg_rating": {
                  "avg": 2.5
                }
              },
              {
                "id": 2,
                "name": "Beltran",
                "avg_rating": {
                  "avg": 3
                }
              },
              {
                "id": 3,
                "name": "Sidney",
                "avg_rating": {
                  "avg": 2.6666666666666665
                }
              }
            ]
          }
        }

  .. tab:: API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { authors { id name avg_rating { avg }}}"
      }
