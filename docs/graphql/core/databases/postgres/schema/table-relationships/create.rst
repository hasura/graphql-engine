.. meta::
   :description: Create relationships between Postgres tables/views in Hasura
   :keywords: hasura, docs, postgres, schema, relationship, create

.. _create_relationships:

Postgres: Creating relationships
================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

A relationship from one table/view to another can be created by defining a link between a column of the table/view to a
column of the other table/view.

Typically, relationships are defined using foreign-key constraints. But in some cases, it might not be possible to
use foreign-key constraints to create the relation. For example, while trying to create a relationship involving a view
as foreign-keys can't be created on views.

.. _relationships-using-fkey:

Using foreign keys
------------------

Say we created two tables, ``authors(id, name)`` and ``articles(id, title, content, rating, author_id)``.

Let us now connect these tables to enable nested queries using a foreign-key:

Step 1: Add foreign-key constraint
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's add a foreign-key constraint to the ``author_id`` column in the ``articles`` table.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    In the console, navigate to the ``Modify`` tab of the ``articles`` table. Click the ``Add`` button in
    the Foreign Keys section and configure the ``author_id`` column as a foreign-key for the ``id`` column in
    the ``authors`` table:

    .. thumbnail:: /img/graphql/core/schema/add-foreign-key.png
      :alt: Add foreign-key constraint

  .. tab:: CLI

    :ref:`Create a migration manually <manual_migrations>` and add the following SQL statement to the ``up.sql`` file:

    .. code-block:: sql

      ALTER TABLE articles
      ADD FOREIGN KEY (author_id) REFERENCES authors(id);

    Add the following statement to the ``down.sql`` file in case you need to :ref:`roll back <roll_back_migrations>` the above statement:

    .. code-block:: sql

      ALTER TABLE articles
      DROP CONSTRAINT articles_author_id_fkey;

    Apply the migration by running:

    .. code-block:: bash

      hasura migrate apply

  .. tab:: API

    You can add a foreign-key constraint using the :ref:`run_sql metadata API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "ALTER TABLE articles ADD FOREIGN KEY (author_id) REFERENCES authors(id);"
        }
      }

Step 2: Create an object relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Each article has one author. This is an ``object relationship``.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    The console infers potential relationships using the foreign-key created above and recommends these in the
    ``Relationships`` tab of the ``articles`` table.

    Add an ``object relationship`` named ``author`` for the ``articles`` table as shown here:

    .. thumbnail:: /img/graphql/core/schema/add-1-1-relationship.png
      :alt: Create an object relationship

  .. tab:: CLI

    You can add an object relationship in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
       :emphasize-lines: 4-7

        - table:
            schema: public
            name: articles
          object_relationships:
          - name: author
            using:
              foreign_key_constraint_on: author_id
        - table:
            schema: public
            name: authors

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

    You can create an object relationship by using the :ref:`create_object_relationship metadata API <create_object_relationship>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "create_object_relationship",
        "args": {
          "table": "articles",
          "name": "author",
          "using": {
            "foreign_key_constraint_on": "author_id"
          }
        }
      }

We can now run a nested object query that is based on this ``object relationship``.

Fetch a list of articles and each article's author:

.. graphiql::
  :view_only:
  :query:
    query {
      articles {
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
        "articles": [
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

Step 3: Create an array relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An author can write multiple articles. This is an ``array relationship``.

You can add an ``array relationship`` in the same fashion as an ``object relationship`` as shown above.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    On the console, add an ``array relationship`` named ``articles`` for the ``authors`` table as shown here:

    .. thumbnail:: /img/graphql/core/schema/add-1-many-relationship.png
      :alt: Create an array relationship

    We can now run a nested object query that is based on this ``array relationship``.

  .. tab:: CLI

    You can add an array relationship in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
      :emphasize-lines: 11-18

        - table:
            schema: public
            name: articles
          object_relationships:
          - name: author
            using:
              foreign_key_constraint_on: author_id
        - table:
            schema: public
            name: authors
          array_relationships:
          - name: articles
            using:
              foreign_key_constraint_on:
                column: author_id
                table:
                  schema: public
                  name: articles

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

    You can create an array relationship by using the :ref:`create_array_relationship metadata API <create_array_relationship>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "create_array_relationship",
        "args": {
          "table": "authors",
          "name": "articles",
          "using": {
            "foreign_key_constraint_on" : {
              "table" : "articles",
              "column" : "author_id"
            }
          }
        }
      }

Fetch a list of authors and a nested list of each author's articles:

.. graphiql::
  :view_only:
  :query:
    query {
      authors {
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
        "authors": [
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

.. _create_manual_relationships:

Using manual relationships
--------------------------

Let's say you have a table ``authors (id, name)`` and a :ref:`view <custom_views>` ``author_avg_rating (id, avg)`` which has the
average rating of articles for each author.

Let us now create an ``object relationship`` called ``avg_rating`` from the ``authors`` table to the
``author_avg_rating`` view using a manual relationship:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    **Step 1: Open the manual relationship section**

    - Open the console and navigate to the ``Data -> authors -> Relationships`` tab.
    - Click on the ``Configure`` button:

    .. thumbnail:: /img/graphql/core/schema/manual-relationship-btn.png
      :alt: Open the manual relationship section

    **Step 2: Define the relationship**

    The above step will open up a section as shown below:

    .. thumbnail:: /img/graphql/core/schema/manual-relationship-create.png
      :alt: Define the relationship

    In this case:

    - **Relationship Type** will be: ``Object Relationship``
    - **Relationship Name** can be: ``avg_rating``
    - **Reference** will be: ``id -> author_avg_rating . id`` *(similar to defining a foreign-key)*

    **Step 3: Create the relationship**

    Now click on the ``Save`` button to create the relationship.

  .. tab:: CLI

    You can add a manual relationship in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
       :emphasize-lines: 7-15

        - table:
            schema: public
            name: articles
        - table:
            schema: public
            name: authors
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

  .. tab:: API

    You can add a manual relationship by using the :ref:`create_object_relationship metadata API <create_object_relationship>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "create_object_relationship",
        "args": {
          "table": "authors",
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

.. graphiql::
  :view_only:
  :query:
    query {
      authors {
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
        "authors": [
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

Tracking existing relationships inferred via foreign-keys
---------------------------------------------------------

As mentioned in the Introduction section above, relationships can be inferred via foreign-keys that exist in your database:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    The console infers potential relationships using existing foreign-keys and recommends these on the ``Data -> Schema`` page

    .. thumbnail:: /img/graphql/core/schema/schema-track-relationships.png
      :alt: Track all relationships

    You can choose to track the relationships individually using the ``Track`` buttons or hit the ``Track all`` button to
    track all the inferred relationships in one go.

  .. tab:: CLI

    You can add relationships in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
      :emphasize-lines: 4-7,11-18

      - table:
          schema: public
          name: articles
        object_relationships:
        - name: author
          using:
            foreign_key_constraint_on: author_id
      - table:
          schema: public
          name: authors
        array_relationships:
        - name: articles
          using:
            foreign_key_constraint_on:
              column: author_id
              table:
                schema: public
                name: articles

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

    You can create multiple relationships by using the :ref:`create_object_relationship metadata API <create_object_relationship>`
    and the :ref:`create_array_relationship metadata API <create_array_relationship>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "bulk",
        "args": [
          {
            "type": "create_object_relationship",
            "args": {
              "table": "articles",
              "name": "author",
              "using": {
                "foreign_key_constraint_on": "author_id"
              }
            }
          },
          {
            "type": "create_array_relationship",
            "args": {
              "table": "authors",
              "name": "articles",
              "using": {
                "foreign_key_constraint_on" : {
                  "table" : "articles",
                  "column" : "author_id"
                }
              }
            }
          }
        ]
      }


