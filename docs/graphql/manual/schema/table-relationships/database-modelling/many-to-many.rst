.. meta::
   :description: Model many-to-many relationships in Hasura
   :keywords: hasura, docs, schema, relationship, many-to-many, n-m

.. _many_to_many_modelling:

Modelling many-to-many table relationships
==========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

A ``many-to-many`` relationship between two tables can be established by creating a table typically called as
**bridge/junction/join table** and adding **foreign-key constraints** from it to the original tables.

Say we have the following two tables in our database schema:

.. code-block:: sql

  article (
    id SERIAL PRIMARY KEY,
    title TEXT
    ...
  )

  tag (
    id SERIAL PRIMARY KEY,
    tag_value TEXT
    ...
  )

These two tables are related via a ``many-to-many`` relationship. i.e:

- an ``article`` can have many ``tags``
- a ``tag`` has many ``articles``

Set up a table relationship in the database
-------------------------------------------

This ``many-to-many`` relationship can be established in the database by:

1. Creating a **bridge table** called ``article_tag`` with the following structure:

   .. code-block:: sql

      article_tag (
        article_id INT
        tag_id INT
        PRIMARY KEY (article_id, tag_id)
        ...
      )

2. Adding **foreign key constraints** from the ``article_tag`` table to:

   - the ``article`` table using the ``article_id`` and ``id`` columns of the tables respectively
   - the ``tag`` table using the ``tag_id`` and ``id`` columns of the tables respectively


The table ``article_tag`` sits between the two tables involved in the many-to-many relationship and captures possible
permutations of their association via the foreign keys.

Set up GraphQL relationships
----------------------------

To access the nested objects via the GraphQL API, :ref:`create the following relationships <create_relationships>`:

- Array relationship, ``article_tags`` from ``article`` table using  ``article_tag :: article_id -> id``
- Object relationship, ``tag`` from ``article_tag`` table using  ``tag_id -> tag :: id``
- Array relationship, ``tag_articles`` from ``tag`` table using  ``article_tag :: tag_id -> id``
- Object relationship, ``article`` from ``article_tag`` table using  ``article_id -> article :: id``

Query using relationships
-------------------------

We can now:

- fetch a list of articles with their tags:

  .. graphiql::
    :view_only:
    :query:
      query {
        article {
          id
          title
          article_tags {
            tag {
              id
              tag_value
            }
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
              "article_tags": [
                {
                  "tag": {
                    "id": 1,
                    "tag_value": "mystery"
                  }
                },
                {
                  "tag": {
                    "id": 2,
                    "tag_value": "biography"
                  }
                }
              ]
            },
            {
              "id": 2,
              "title": "a nibh",
              "article_tags": [
                {
                  "tag": {
                    "id": 2,
                    "tag_value": "biography"
                  }
                },
                {
                  "tag": {
                    "id": 5,
                    "tag_value": "technology"
                  }
                }
              ]
            }
          ]
        }
      }

- fetch a list of tags with their articles:

  .. graphiql::
    :view_only:
    :query:
      query {
        tag {
          id
          tag_value
          tag_articles {
            article {
              id
              title
            }
          }
        }
      }
    :response:
      {
        "data": {
          "tag": [
            {
              "id": 1,
              "tag_value": "mystery",
              "tag_articles": [
                {
                  "article": {
                    "id": 1,
                    "title": "sit amet"
                  }
                }
              ]
            },
            {
              "id": 2,
              "tag_value": "biography",
              "tag_articles": [
                {
                  "article": {
                    "id": 1,
                    "title": "sit amet"
                  }
                },
                {
                  "article": {
                    "id": 2,
                    "title": "a nibh"
                  }
                }
              ]
            }
          ]
        }
      }

Fetching relationship information
---------------------------------

The intermediate fields ``article_tags`` & ``tag_articles`` can be used to fetch extra
information about the relationship. For example, you can have a column like ``tagged_at`` in the ``article_tag``
table which you can fetch as follows:

.. graphiql::
  :view_only:
  :query:
    query {
      article {
        id
        title
        article_tags {
          tagged_at
          tag {
            id
            tag_value
          }
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
            "article_tags": [
              {
                "tagged_at": "2018-11-19T18:01:17.292828+05:30",
                "tag": {
                  "id": 1,
                  "tag_value": "mystery"
                }
              },
              {
                "tagged_at": "2018-11-18T18:01:17.292828+05:30",
                "tag": {
                  "id": 3,
                  "tag_value": "romance"
                }
              }
            ]
          },
          {
            "id": 2,
            "title": "a nibh",
            "article_tags": [
              {
                "tagged_at": "2018-11-19T15:01:17.292828+05:30",
                "tag": {
                  "id": 5,
                  "tag_value": "biography"
                }
              },
              {
                "tagged_at": "2018-11-16T14:01:17.292828+05:30",
                "tag": {
                  "id": 3,
                  "tag_value": "romance"
                }
              }
            ]
          }
        ]
      }
    }


Flattening a many-to-many relationship query
--------------------------------------------

In case you would like to flatten the above queries and avoid the intermediate fields ``article_tags`` &
``tag_articles``, you can :ref:`create the following views <custom_views>` additionally and then
query using relationships created on these views:

.. code-block:: sql

  CREATE VIEW article_tags_view AS
    SELECT article_id, tag.*
      FROM article_tag LEFT JOIN tag
        ON article_tag.tag_id = tag.id

  CREATE VIEW tag_articles_view AS
    SELECT tag_id, article.*
      FROM article_tag LEFT JOIN article
        ON article_tag.article_id = article.id

Now :ref:`create the following relationships <create_relationships>`:

- Array relationship, ``tags`` from the ``article`` table using  ``article_tags_view :: article_id -> id``
- Array relationship, ``articles`` from the ``tag`` table using  ``tag_articles_view :: tag_id -> id``

We can now:

- fetch articles with their tags without an intermediate field:

  .. graphiql::
    :view_only:
    :query:
      query {
        article {
          id
          title
          tags {
            id
            tag_value
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
              "tags": [
                {
                  "id": 1,
                  "tag_value": "mystery"
                },
                {
                  "id": 3,
                  "tag_value": "romance"
                }
              ]
            },
            {
              "id": 2,
              "title": "a nibh",
              "tags": [
                {
                  "id": 5,
                  "tag_value": "biography"
                },
                {
                  "id": 3,
                  "tag_value": "romance"
                }
              ]
            }
          ]
        }
      }

  - fetch tags with their articles without an intermediate field:

    .. graphiql::
      :view_only:
      :query:
        query {
          tag {
            id
            tag_value
            articles {
              id
              title
            }
          }
        }
      :response:
        {
          "data": {
            "tag": [
              {
                "id": 1,
                "tag_value": "mystery",
                "articles": [
                  {
                    "id": 1,
                    "title": "sit amet"
                  }
                ]
              },
              {
                "id": 2,
                "tag_value": "biography",
                "articles": [
                  {
                    "id": 1,
                    "title": "sit amet"
                  },
                  {
                    "id": 2,
                    "title": "a nibh"
                  }
                ]
              }
            ]
          }
        }

.. note::

  **We do not recommend this** flattening pattern of modelling as this introduces an additional overhead of managing
  permissions and relationships on the newly created views. e.g. You cannot query for the author of the nested articles
  without setting up a new relationship to the ``author`` table from the ``tag_articles_view`` view.

  In our opinion, the cons of this approach seem to outweigh the pros.
