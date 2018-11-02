Paginate query results
======================
The operators :ref:`limit <PaginationExp>` and :ref:`offset <PaginationExp>` are used for pagination.
``limit`` specifies the number of rows to retain from the result set
and ``offset`` determines which slice to retain from the results.

The following are examples of pagination in different scenarios:

Limit results
-------------
Fetch the first 5 authors from the list of all authors:

.. graphiql::
  :view_only:
  :query:
    query {
      author(
        limit: 5
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
            "id": 1,
            "name": "Justin"
          },
          {
            "id": 2,
            "name": "Beltran"
          },
          {
            "id": 3,
            "name": "Sidney"
          },
          {
            "id": 4,
            "name": "Anjela"
          },
          {
            "id": 5,
            "name": "Amii"
          }
        ]
      }
    }

Limit results from an offset
----------------------------
Fetch 5 authors from the list of all authors, starting with the 6th one:

.. graphiql::
  :view_only:
  :query:
    query {
      author(
        limit: 5,
        offset:5
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
            "id": 6,
            "name": "Corny"
          },
          {
            "id": 7,
            "name": "Berti"
          },
          {
            "id": 8,
            "name": "April"
          },
          {
            "id": 9,
            "name": "Ninnetta"
          },
          {
            "id": 10,
            "name": "Lyndsay"
          }
        ]
      }
    }

Limit results in a nested object
--------------------------------
Fetch a list of authors and a list of 2 of each of their articles:

.. graphiql::
  :view_only:
  :query:
    query {
      author {
        id
        name
        articles (
          limit:2
        ) {
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
          },
          {
            "id": 3,
            "name": "Sidney",
            "articles": [
              {
                "id": 6,
                "title": "sapien ut"
              },
              {
                "id": 11,
                "title": "turpis eget"
              }
            ]
          },
          {
            "id": 4,
            "name": "Anjela",
            "articles": [
              {
                "id": 1,
                "title": "sit amet"
              },
              {
                "id": 3,
                "title": "amet justo morbi"
              }
            ]
          }
        ]
      }
    }
