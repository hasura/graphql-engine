Aggregation queries
===================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can fetch aggregations on columns along with nodes using an aggregation query.
Available aggregation functions are ``count``, ``sum``, ``avg``, ``max`` and ``min``.

The **name of the aggregate field** is of the form ``<field-name> + _aggregate``.

You can see the complete specification of the aggregate field in the :ref:`API reference <AggregateObject>`.

Fetch aggregated data of an object
----------------------------------

**Example:** Fetch a list of articles with aggregated data of their rating:

.. graphiql::
  :view_only:
  :query:
    query {
      article_aggregate {
        aggregate {
          count
          sum {
            rating
          }
          avg {
            rating
          }
          max {
            rating
          }
        }
        nodes {
          id
          title
          rating
        }
      }
    }
  :response:
    {
      "data": {
        "article_aggregate": {
          "aggregate": {
            "count": 10,
            "sum": {
              "rating": 26
            },
            "avg": {
              "rating": 2.6
            },
            "max": {
              "rating": 4
            }
          },
          "nodes": [
            {
              "id": 1,
              "title": "sit amet",
              "rating": 1
            },
            {
              "id": 2,
              "title": "a nibh",
              "rating": 3
            },
            {
              "id": 3,
              "title": "amet justo morbi",
              "rating": 4
            },
            {
              "id": 4,
              "title": "vestibulum ac est",
              "rating": 2
            },
            {
              "id": 5,
              "title": "ut blandit",
              "rating": 2
            },
            {
              "id": 6,
              "title": "sapien ut",
              "rating": 1
            },
            {
              "id": 7,
              "title": "nisl duis ac",
              "rating": 4
            },
            {
              "id": 8,
              "title": "donec semper sapien",
              "rating": 3
            },
            {
              "id": 9,
              "title": "sit amet",
              "rating": 3
            },
            {
              "id": 10,
              "title": "dui proin leo",
              "rating": 3
            }
          ]
        }
      }
    }

.. _nested_aggregate:

Fetch aggregated data on nested objects
---------------------------------------
The following is an example of a nested object query with aggregations on the **array relationship** between an author
and articles.

**Example:** Fetch author with id "1" and a nested list of articles with aggregated data of their rating:

.. graphiql::
  :view_only:
  :query:
    query {
      author (where: {id: {_eq: 1}}) {
        id
        name
        articles_aggregate {
          aggregate {
            count
            avg {
              rating
            }
            max {
              rating
            }
          }
          nodes {
            id
            title
            rating
          }
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
            "articles_aggregate": {
              "aggregate": {
                "count": 2,
                "avg": {
                  "rating": 2.5
                },
                "max": {
                  "rating": 4
                }
              },
              "nodes": [
                {
                  "id": 15,
                  "title": "vel dapibus at",
                  "rating": 4
                },
                {
                  "id": 16,
                  "title": "sem duis aliquam",
                  "rating": 1
                }
              ]
            }
          }
        ]
      }
    }

