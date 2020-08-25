.. meta::
   :description: Make simple object queries in Hasura
   :keywords: hasura, docs, query, object query

.. _simple_object_queries:

Simple object queries
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

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
**Example:** Fetch the city and phone number of an author from their JSONB
address column:

.. graphiql::
  :view_only:
  :query:
    query {
      author_by_pk(id: 1) {
        id
        name
        address
        city: address(path: "$.city")
        phone: address(path: "$.phone_numbers.[0]")
      }
    }
  :response:
    {
      "data": {
        "author_by_pk": {
          "id": 1,
          "name": "Justin",
          "address": {
            "city": "Bengaluru",
            "phone_numbers": [9090909090, 8080808080]
          },
          "city": "Bengaluru",
          "phone": 9090909090
        }
      }
    }

See the :ref:`API reference <SimpleObject>` for more details.
