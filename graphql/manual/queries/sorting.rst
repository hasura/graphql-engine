Sort query results
==================
Results from your query can be sorted by using the ``order_by`` argument. The argument can be used to sort based on fields in nested objects too. The sort order (ascending vs. descending) is set by specifying the ``+`` or ``-`` in front of the column name in the ``order_by`` argument. The ``order_by`` argument takes an array of parameters to allow sorting by multiple columns. The following are examples of sorting different types of queries.

Sorting in a simple object query
--------------------------------
Fetch a list of authors that is sorted by their names in a descending order:

.. graphiql::
  :query:
    query {
      author(order_by: ["-name"]) {
        id
        name
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 26,
            "name": "Wenda"
          },
          {
            "id": 21,
            "name": "Sophey"
          },
          {
            "id": 20,
            "name": "Saunderson"
          },
          {
            "id": 11,
            "name": "Rubi"
          },
          {
            "id": 12,
            "name": "Ricoriki"
          },
          {
            "id": 13,
            "name": "Quintus"
          }
        ]
      }
    }

Sorting a nested object query
-----------------------------
Fetch a list of authors that is sorted by their names and a list of each of their articles that is sorted in the reverse chronological order of their publication date:

.. graphiql::
  :query:
    query {
      author(order_by: ["-id"]) {
        id
        name
        articles (order_by: ["-published_on"]) {
          id
          published_on
        }
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 29,
            "name": "Carmella",
            "articles": [
              {
                "id": 78,
                "published_on": null
              },
              {
                "id": 64,
                "published_on": null
              }
            ]
          },
          {
            "id": 28,
            "name": "Derril",
            "articles": [
              {
                "id": 3,
                "published_on": null
              },
              {
                "id": 10,
                "published_on": null
              },
              {
                "id": 34,
                "published_on": null
              },
              {
                "id": 38,
                "published_on": null
              },
              {
                "id": 59,
                "published_on": null
              },
              {
                "id": 88,
                "published_on": null
              }
            ]
          },
          {
            "id": 27,
            "name": "Ashby",
            "articles": [
              {
                "id": 17,
                "published_on": null
              },
              {
                "id": 7,
                "published_on": null
              },
              {
                "id": 99,
                "published_on": null
              }
            ]
          }
        ]
      }
    }

Sorting by multiple fields
--------------------------
Fetch a list of articles that is sorted by the date they were published (descending) and then on their title(alphabetically):

.. graphiql::
  :query:
    query {
      article(order_by: ["-published_on", "+title"]) {
        id
        title
        content
        published_on
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 2,
            "title": "a some title",
            "content": "some content",
            "published_on": "2018-06-14"
          },
          {
            "id": 6,
            "title": "some title",
            "content": "some content",
            "published_on": "2018-06-11"
          }
        ]
      }
    }