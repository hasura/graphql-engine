Simple object queries
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can fetch a single node or multiple nodes of the same type using a simple object query.

Fetch list of objects
---------------------
**Example:** Fetch a list of authors:

.. graphiql::
  :view_only:
  :query:
    query {
      author {
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
          }
        ]
      }
    }
