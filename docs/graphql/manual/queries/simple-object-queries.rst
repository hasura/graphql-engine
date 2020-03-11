.. meta::
   :description: Make simple object queries in Hasura
   :keywords: hasura, docs, query, object query

Simple object queries
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
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


Fetch an object using its primary key
-------------------------------------
**Example:** Fetch an author using their primary key:

.. graphiql::
  :view_only:
  :query:
    query {
      author_by_pk(id: 1) {
        id
        name
      }
    }
  :response:
    {
      "data": {
        "author_by_pk": {
          "id": 1,
          "name": "Justin"
        }
      }
    }

Fetch value from JSON/JSONB column at particular path
-----------------------------------------------------
**Example:** Fetch pincode from address for a user where address is a JSON/JSONB column

.. graphiql::
  :view_only:
  :query:
    query {
      user_by_pk(id: 1) {
        id
        name
        pincode: address(path: ".pincode") # pincode is an alias
      }
    }
  :response:
    {
      "data": {
        "user_by_pk": {
          "id": 1,
          "name": "Justin",
          "pincode": 110017
        }
      }
    }
