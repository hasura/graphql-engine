Sort query results
==================
Results from your query can be sorted by using the ``order_by`` argument. The argument can be used to sort based on
fields in nested objects too. The sort order (ascending vs. descending) is set by specifying the ``+`` or ``-`` in
front of the column name in the ``order_by`` argument. The ``order_by`` argument takes an array of parameters to allow
sorting by multiple columns. The following are examples of sorting different types of queries.

Sorting in a simple object query
--------------------------------
Fetch list of authors sorted by their names in an ascending order:

.. graphiql::
  :query:
    query {
      author(
        order_by: ["+name"]
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

Sorting a nested object query
-----------------------------
Fetch a list of authors sorted by their names with a list of their articles that is sorted by their rating:

.. graphiql::
  :query:
    query {
      author(order_by: ["+name"]) {
        id
        name
        articles(order_by: ["-rating"]) {
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

Sorting by multiple fields
--------------------------
Fetch a list of articles that is sorted by their rating (descending) and then on their published date (ascending):

.. graphiql::
  :query:
    query {
      article(
        order_by: ["-rating", "+published_on"]
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
            "id": 7,
            "rating": 4,
            "published_on": "2016-07-09"
          },
          {
            "id": 3,
            "rating": 4,
            "published_on": "2017-05-26"
          },
          {
            "id": 15,
            "rating": 4,
            "published_on": "2018-01-02"
          }
        ]
      }
    }