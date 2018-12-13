Sort query results
==================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Results from your query can be sorted by using the :ref:`order_by <OrderByExp>` argument. The argument can be used to sort nested
objects too.

The sort order (ascending vs. descending) is set by specifying ``asc`` or ``desc``
enum value for the column name in the ``order_by`` input object e.g. ``{name: desc}``.

By default, for ascending ordering ``null`` values are returned at the end of the results and for descending ordering ``null``
values are returned at the start of the results. ``null`` values can be fetched first on ascending ordering by specifying
``asc_nulls_first`` and last on descending ordering by specifying ``desc_nulls_last`` enum value e.g. ``{name: desc_nulls_last}``.

The ``order_by`` argument takes an array of objects to allow sorting by multiple columns.

.. code-block:: graphql

   article (
     order_by: [article_order_by!]
   ): [article]!

   #order by type for "article" table
   input article_order_by {
     id: order_by
     title: order_by
     content: order_by
     author_id: order_by
     #order by using "author" object relationship columns
     author: author_order_by
     #order by using "likes" array relationship aggregates
     likes_aggregate: likes_aggregate_order_by
   }

   #the likes_aggregate_order_by type
   input likes_aggregate_order_by {
     count: order_by
     op_name: likes_op_name_order_by
   }
   #Available "op_name"s are 'max', 'min', 'sum', 'avg', 'stddev', 'stddev_samp', 'stddev_pop', 'variance', 'var_samp' and 'var_pop'

   #the likes_<op-name>_order_by type
   input likes_sum_order_by {
     like_id: order_by
   }

   #the order_by enum type
   enum order_by {
     #in the ascending order, nulls last
     asc
     #in the ascending order, nulls last
     asc_nulls_last
     #in the ascending order, nulls first
     asc_nulls_first
     #in the descending order, nulls first
     desc
     #in the descending order, nulls first
     desc_nulls_first
     #in the descending order, nulls last
     desc_nulls_last
   }


.. Note::
   Only columns from **object** relationships are allowed for sorting.
   Only aggregates from **array** relationships are allowed for sorting.

The following are example queries for different sorting use cases:

Sorting objects
---------------

Fetch list of authors sorted by their names in an ascending order:

.. graphiql::
  :view_only:
  :query:
    query {
      author(
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
      author(order_by: {name: asc}) {
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

Sorting objects based on nested object's fields
-----------------------------------------------
Fetch a list of articles that is sorted by their author's id (descending).
Only columns in object relationships are allowed:

.. graphiql::
  :view_only:
  :query:
    query {
      article(
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

Sorting by array relationship aggregates
----------------------------------------
Fetch a list of authors sorted by their article count.

.. graphiql::
  :view_only:
  :query:
    query {
      author(order_by: {articles_aggregate: {count: desc}}) {
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
      article(
        order_by: [{rating: desc}, {published_on: asc_nulls_first}]
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
