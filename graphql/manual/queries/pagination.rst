Paginate query results
======================
The operators ``limit`` and ``offset`` are used for pagination, etc. ``limit`` specifies the number of rows to retain from the result set and ``offset`` determines which slice to retain from the results. The following are examples of pagination in different scenarios.

Limit results
-------------
Fetch the first 5 authors from a list of all authors:

.. graphiql::
  :query:
    query {
      article(limit: 5) {
        id
        title
        content
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 3,
            "title": "some title",
            "content": "some content"
          },
          {
            "id": 4,
            "title": "some title",
            "content": "some content"
          },
          {
            "id": 5,
            "title": "some title",
            "content": "some content"
          },
          {
            "id": 6,
            "title": "some title",
            "content": "some content"
          },
          {
            "id": 8,
            "title": "some title",
            "content": "some content"
          }
        ]
      }
    }

Limit results from an offset
----------------------------
Fetch 5 authors from a list of all authors, starting with the 11th one:

.. graphiql::
  :query:
    query {
      article(limit: 5, offset:10) {
        id
        title
        content
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 14,
            "title": "some title",
            "content": "some content"
          },
          {
            "id": 78,
            "title": "some title",
            "content": "some content"
          },
          {
            "id": 79,
            "title": "some title",
            "content": "some content"
          },
          {
            "id": 80,
            "title": "some title",
            "content": "some content"
          },
          {
            "id": 22,
            "title": "some title",
            "content": "some content"
          }
        ]
      }
    }

Limit results in a nested object
--------------------------------
Fetch a list of authors and a list of 5 of each of their most recently published articles:

.. graphiql::
  :query:
    query {
      author {
        id
        name
        articles (order_by: ["-published_on"], limit:5)  {
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
            "id": 1,
            "name": "Chrissie",
            "articles": [
              {
                "id": 73,
                "published_on": null
              },
              {
                "id": 87,
                "published_on": null
              },
              {
                "id": 98,
                "published_on": null
              }
            ]
          },
          {
            "id": 20,
            "name": "Saunderson",
            "articles": [
              {
                "id": 60,
                "published_on": null
              },
              {
                "id": 6,
                "published_on": "2018-06-11"
              }
            ]
          },
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
          }
        ]
      }
    }