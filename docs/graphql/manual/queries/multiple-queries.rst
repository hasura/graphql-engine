Multiple queries in a request
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

If multiple queries are part of the same request, they are executed **parallelly**, the individual responses are
collated and returned. You can fetch objects of different unrelated types in the same query.

For example, fetch a list of ``authors`` and a list of ``articles``:

.. graphiql::
  :view_only:
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