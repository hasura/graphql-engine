Simple object queries
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can fetch a single node or multiple nodes of the same type using a simple object query.

Fetch list of objects
---------------------
For example, fetch a list of authors:

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


Fetch JSON fields
---------------------

For example, each author has addresses that are stored in ``contacts`` JSON field. We can query address properties with ``path`` argument:

.. graphiql::
  :view_only:
  :query:
    query {
      author {
        addresses: contacts(path: ".addresses")
        city: contacts(path: ".addresses[0].city")
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "addresses": [{
              "street": "19th Main Road",
              "city": "Bangalore",
              "postal": "560095"
            }],
            "city": "Bangalore"
          }
        ]
      }
    }
