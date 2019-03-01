Sort query results
==================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Results from your query can be sorted by using the ``order_by`` argument. The argument can be used to sort nested
objects too.

The sort order (ascending vs. descending) is set by specifying ``asc`` or ``desc``
enum value for the column name in the ``order_by`` input object e.g. ``{name: desc}``.

By default, for ascending ordering ``null`` values are returned at the end of the results and for descending
ordering ``null`` values are returned at the start of the results. ``null`` values can be fetched first on
ascending ordering by specifying ``asc_nulls_first`` and last on descending ordering by specifying
``desc_nulls_last`` enum value e.g. ``{name: desc_nulls_last}``.

The ``order_by`` argument takes an array of objects to allow sorting by multiple columns.

You can also use nested objects' fields to sort the results. Only columns from **object relationships** and
are aggregates from **array relationships** can be used for sorting.

You can see the complete specification of the ``order_by`` argument in the :ref:`API reference <OrderByExp>`.

The following are example queries for different sorting use cases:

Sorting objects
---------------

Fetch list of authors sorted by their names in an ascending order:

.. graphiql::
  :view_only:
  :query:
    query {
      author (
        order_by: {name: asc}
      ) {
        id
        name
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 5,
            "name": "Amii"
          },
          {
            "id": 4,
            "name": "Anjela"
          },
          {
            "id": 8,
            "name": "April"
          },
          {
            "id": 2,
            "name": "Beltran"
          },
          {
            "id": 7,
            "name": "Berti"
          },
          {
            "id": 6,
            "name": "Corny"
          }
        ]
      }
    }

Sorting nested objects
----------------------
Fetch a list of authors sorted by their names with a list of their articles that is sorted by their rating:

.. graphiql::
  :view_only:
  :query:
    query {
      author (order_by: {name: asc}) {
        id
        name
        articles(order_by: {rating: desc}) {
          id
          title
          rating
        }
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 5,
            "name": "Amii",
            "articles": [
              {
                "rating": 5,
                "id": 17,
                "title": "montes nascetur ridiculus"
              },
              {
                "rating": 3,
                "id": 12,
                "title": "volutpat quam pede"
              },
              {
                "rating": 2,
                "id": 4,
                "title": "vestibulum ac est"
              }
            ]
          },
          {
            "id": 4,
            "name": "Anjela",
            "articles": [
              {
                "rating": 4,
                "id": 3,
                "title": "amet justo morbi"
              },
              {
                "rating": 1,
                "id": 1,
                "title": "sit amet"
              }
            ]
          },
          {
            "id": 8,
            "name": "April",
            "articles": [
              {
                "rating": 4,
                "id": 13,
                "title": "vulputate elementum"
              },
              {
                "rating": 2,
                "id": 20,
                "title": "eu nibh"
              }
            ]
          }
        ]
      }
    }

Sorting based on nested object's fields
---------------------------------------
Only columns in object relationships can be used for sorting.

Fetch a list of articles that are sorted by their author ids in descending:

.. graphiql::
  :view_only:
  :query:
    query {
      article (
        order_by: {author: {id: desc}}
      ) {
        id
        rating
        published_on
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
            "id": 3,
            "title": "Article 3",
            "content": "Sample article content 3",
            "author": {
              "id": 2,
              "name": "Author 2"
            }
          },
          {
            "id": 1,
            "title": "Article 1",
            "content": "Sample article content 1",
            "author": {
              "id": 1,
              "name": "Author 1"
            }
          },
          {
            "id": 2,
            "title": "Article 2",
            "content": "Sample article content 2",
            "author": {
              "id": 1,
              "name": "Author 1"
            }
          }
        ]
      }
    }

Sorting based on nested objects' aggregates
-------------------------------------------
Only aggregates in array relationships can be used for sorting.

Fetch a list of authors sorted by their article count in descending.

.. graphiql::
  :view_only:
  :query:
    query {
      author (
        order_by: {articles_aggregate: {count: desc}}
      ) {
        id
        name
        articles_aggregate {
          aggregate{
            count
          }
        }
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 5,
            "name": "Amii",
            "articles_aggregate":{
              "aggregate": {
                "count": 3
              }
            }
          },
          {
            "id": 4,
            "name": "Anjela",
            "articles_aggregate":{
              "aggregate": {
                "count": 2
              }
            }
          },
          {
            "id": 8,
            "name": "April",
            "articles_aggregate":{
              "aggregate": {
                "count": 2
              }
            }
          }
        ]
      }
    }


Sorting by multiple fields
--------------------------
Fetch a list of articles that is sorted by their rating (descending) and then on their published date (ascending with
nulls first):

.. graphiql::
  :view_only:
  :query:
    query {
      article (
        order_by: [
          {rating: desc},
          {published_on: asc_nulls_first}
        ]
      ) {
        id
        rating
        published_on
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 17,
            "rating": 5,
            "published_on": null
          },
          {
            "id": 14,
            "rating": 4,
            "published_on": null
          },
          {
            "id": 7,
            "rating": 4,
            "published_on": "2016-07-09"
          },
          {
            "id": 3,
            "rating": 4,
            "published_on": "2017-05-26"
          }
        ]
      }
    }
