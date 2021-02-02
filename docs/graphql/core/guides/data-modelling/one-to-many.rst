.. meta::
   :description: Model one-to-many relationships in Hasura
   :keywords: hasura, docs, schema, relationship, one-to-many, 1-n

.. _one_to_many_modelling:

Modelling one-to-many table relationships
=========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

A ``one-to-many`` relationship between two tables can be established via a **foreign key constraint**.

Say we have the following two tables in our database schema:

.. code-block:: sql

  author (
    id SERIAL PRIMARY KEY,
    name TEXT
  )

  article (
    id SERIAL PRIMARY KEY,
    author_id INT
    title TEXT
    ...
  )

These two tables are related via a ``one-to-many`` relationship. i.e:

- an ``author`` can have many ``articles``
- an ``article`` has one ``author``

Step 1: Set up a table relationship in the database
---------------------------------------------------

This ``one-to-many`` relationship can be established in the database by:

1. Adding a **foreign key constraint** from the ``article`` table to the ``author`` table using the ``author_id`` and
   ``id`` columns of the tables respectively.

This will ensure that the value of ``author_id`` column in the ``article`` table  is present in the ``id`` column of
the ``author`` table.

Step 2: Set up GraphQL relationships
------------------------------------

To access the nested objects via the GraphQL API, :ref:`create the following relationships <create_relationships>`:

- Array relationship, ``articles`` from ``author`` table using  ``article :: author_id  ->  id``
- Object relationship, ``author`` from ``article`` table using ``author_id -> author :: id``

Query using one-to-many relationships
-------------------------------------

We can now:

- fetch a list of ``authors`` with their ``articles``:

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
            }
          ]
        }
      }


- fetch a list of ``articles`` with their ``author``:

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
                "id": 4,
                "name": "Anjela"
              }
            },
            {
              "id": 2,
              "title": "a nibh",
              "author": {
                "id": 2,
                "name": "Beltran"
              }
            }
          ]
        }
      }

Insert using one-to-many relationships
--------------------------------------

We can now:
 
- insert an ``author`` with their ``articles`` where the author might already exist (assume unique ``name`` for ``author``):
 
.. graphiql::
  :view_only:
  :query:
    mutation UpsertAuthorWithArticles {
      insert_author(objects: {
        name: "Felix",
        articles: {
          data: [
            {
              title: "Article 1",
              content: "Article 1 content"
            },
            {
              title: "Article 2",
              content: "Article 2 content"
            }
          ]
        }
      },
        on_conflict: {
          constraint: author_name_key,
          update_columns: [name]
        }
      ) {
        returning {
          name
          articles {
            title
            content
          }
        }
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "returning": [
            {
              "name": "Felix",
              "articles": [
                {
                  "title": "Article 1",
                  "content": "Article 1 content"
                },
                {
                  "title": "Article 2",
                  "content": "Article 2 content"
                }
              ]
            }
          ]
        }
      }
    }

- insert ``articles`` with their ``author`` where the ``author`` might already exist (assume unique ``name`` for ``author``):

.. graphiql::
  :view_only:
  :query:
    mutation upsertArticleWithAuthors {
      insert_article(objects: [
        {
          title: "Article 1",
          content: "Article 1 content",
          author: {
            data: {
              name: "Alice"
            },
            on_conflict: {
              constraint: author_name_key,
              update_columns: [name]
            }
          }
        },
        {
          title: "Article 2",
          content: "Article 2 content",
          author: {
            data: {
              name: "Alice"
            },
            on_conflict: {
              constraint: author_name_key,
              update_columns: [name]
            }
          }
        }
      ]) {
        returning {
          title
          content
          author {
            name
          }
        }
      }
    }
  :response:
    {
      "data": {
        "insert_article": {
          "returning": [
            {
              "title": "Article 1",
              "content": "Article 1 content",
              "author": {
                "name": "Alice"
              }
            },
            {
              "title": "Article 2",
              "content": "Article 2 content",
              "author": {
                "name": "Alice"
              }
            }
          ]
        }
      }
    }
 
.. note::
 
 You can avoid the ``on_conflict`` clause if you will never have conflicts.
