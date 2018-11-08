Modelling many-to-many table relationships
==========================================

A many-to-many relationship between two tables can be established by creating a table typically called as ``bridge``
or ``junction`` or ``join`` table and adding foreign-key constraints from it to the original tables.

Say we have the following two tables in our database schema:

.. code-block:: sql

  article (
    id INT PRIMARY KEY,
    title TEXT
    ...
  )

  tag (
    id INT PRIMARY KEY,
    tag_value TEXT
    ...
  )

These two tables are related via a ``many-to-many`` relationship. i.e:

- an ``article`` can have many ``tags``
- a ``tag`` has many ``articles``

This ``many-to-many`` relationship can be established in the database by:

1. Creating a bridge table called ``article_tag`` with the following structure:

   .. code-block:: sql

      article_tag (
        id INT PRIMARY KEY
        article_id INT
        tag_id INT
        ...
      )

2. Adding foreign-key constraints from the ``article_tag`` table to:

   - the ``article`` table using the ``article_id`` and ``id`` columns of the tables respectively
   - the ``tag`` table using the ``tag_id`` and ``id`` columns of the tables respectively


The table ``article_tag`` sits between the two tables involved in the many-to-many relationship and captures possible
permutations of their association via the foreign-keys.

To access the nested objects via the GraphQL API, :doc:`create the following relationships <../create>`:

- Array relationship, ``article_tags`` from ``article`` table using  ``article_tag :: article_id -> id``
- Object relationship, ``tag`` from ``article_tag`` table using  ``tag_id -> tag :: id``
- Array relationship, ``tag_articles`` from ``tag`` table using  ``article_tag :: tag_id -> id``
- Object relationship, ``article`` from ``article_tag`` table using  ``article_id -> article :: id``


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

.. note::

  The intermediate fields ``article_tags`` & ``tag_articles`` are important as they can be used to fetch extra
  information about the relationship. For example, you can have a column like ``tagged_at`` in the ``article_tag``
  table which you can fetch as follows:

  .. code-block:: graphql

      query {
        article {
          id
          title
          article_tags {
            tagged_at
            tag {
              tag_value
            }
          }
        }
      }
