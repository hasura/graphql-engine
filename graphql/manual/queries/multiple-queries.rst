Multiple queries in a request
=============================
If multiple queries are part of the same request, they are executed **parallely**, the individual responses are
collated and returned. You can fetch objects of different unrelated types in the same query.

For e.g. to fetch a list of ``authors`` and a list of ``articles``:

.. graphiql::
  :query:
    query {
      author(limit: 2) {
        id
        name
      }
      article(limit: 2) {
        id
        title
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
          }
        ],
        "article": [
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
    }