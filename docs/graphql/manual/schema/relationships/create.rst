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

The first step is to add a foreign-key constraint on the ``article`` table.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    In the console, navigate to the ``Modify`` tab of the ``article`` table. Click the ``Add`` button in
    the Foreign Keys section and configure the ``author_id`` column as a foreign-key for the ``id`` column in
    the ``author`` table:

    .. thumbnail:: /img/graphql/manual/schema/add-foreign-key.png
      :alt: Add foreign-key constraint

  .. tab:: API

    A foreign-key constraint can be created via the :ref:`run_sql <run_sql>` metadata API. 

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
           "sql": "alter table "public"."article"
                 add constraint "article_author_id_fkey"
                 foreign key ("author_id")
                 references "public"."author"
                 ("id") on update restrict on delete restrict;"
        }
      }

Step 2: Create an object relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Each article has one author. This is an ``object relationship``.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    The console infers this using the foreign-key created above and recommends the potential relationship in the
    ``Relationships`` tab of the ``article`` table.

    Add an ``object relationship`` named ``author`` for the ``article`` table as shown here:

    .. thumbnail:: /img/graphql/manual/schema/add-1-1-relationship.png
      :alt: Create an object relationship

  .. tab:: API

    An object relationship can be created via the :ref:`create_object_relationship <create_object_relationship>` metadata API. 

We can now run a nested object query that is based on this ``object relationship``.

Fetch a list of articles and each article's author:

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

Step 3: Create an array relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An author can write multiple articles. This is an ``array relationship``.

You can add an ``array relationship`` in the same fashion as an ``object relationship`` as shown above.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Add an ``array relationship`` named ``articles`` for the ``author`` table as shown here:

    .. thumbnail:: /img/graphql/manual/schema/add-1-many-relationship.png
      :alt: Create an array relationship

  .. tab:: API

    An array relationship can be created via the :ref:`create_array_relationship <create_array_relationship>` metadata API. 

We can now run a nested object query that is based on this ``array relationship``.

Fetch a list of authors and a nested list of each author's articles:

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

.. _create_manual_relationships:

Using manual relationships
--------------------------

Let's say you have a table ``author (id, name)`` and a view ``author_avg_rating (id, avg)`` which has the
average rating of articles for each author.

Let us now create an ``object relationship`` called ``avg_rating`` from the ``author`` table to the
``author_avg_rating`` view using a manual relationship:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

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

  .. tab:: API

    A manual relationship can be created via the :ref:`metadata API <create_manual_relationship>`. 

We can now run a nested object query that is based on this ``object relationship``.

Fetch a list of authors with the average rating of their articles:

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
