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

- insert an ``author`` with their ``articles``:

  .. graphiql::
    :view_only:
    :query:
      mutation insertAuthorWithArticles {
        insert_author(objects: 
          {
            name: "Jenny",
            articles: {
              data: [
                {
                  title: "Article 1"
                },
                {
                  title: "Article 2"
                }
              ]
            }
          }) {
          returning {
            id
            name
            articles {
              id
              title
              author_id
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
                "id": 4,
                "name": "Jenny",
                "articles": [
                  {
                    "id": 1,
                    "title": "Article 1",
                    "author_id": 4
                  },
                  {
                    "id": 2,
                    "title": "Article 2",
                    "author_id": 4
                  }
                ]
              }
            ]
          }
        }
      }

- insert a list of ``articles`` with their ``author``:

  .. graphiql::
    :view_only:
    :query:
      mutation insertArticlesWithAuthor {
        insert_article(objects: [
          {
            title: "Article 1",
            author: {
              data: {
                name: "Kevin"
              }
            }
          },
          {
            title: "Article 2",
            author: {
              data: {
                name: "Gary"
              }
            }
          }
        ]) {
          returning {
            id
            title
            author_id
            author {
              id
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
                "id": 3,
                "title": "Article 1",
                "author_id": 5,
                "author": {
                  "id": 5,
                  "name": "Kevin"
                }
              },
              {
                "id": 4,
                "title": "Article 2",
                "author_id": 6,
                "author": {
                  "id": 6,
                  "name": "Gary"
                }
              }
            ]
          }
        }
      }
