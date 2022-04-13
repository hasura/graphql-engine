.. meta::
   :description: Run multiple queries in a request in Hasura
   :keywords: hasura, docs, query, multiple queries, request

.. _ms_sql_server_multiple_queries:

Multiple queries in a request
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Execution
---------

If multiple queries are part of the same request, they are executed **sequentially**, the
individual responses are collated and returned together. You can fetch objects of different
unrelated types in the same query.

Run multiple top level queries in the same request
--------------------------------------------------

**For example**, fetch a list of ``authors`` and a list of ``articles``:

.. graphiql::
  :view_only:
  :query:
    query {
      authors(limit: 2) {
        id
        name
      }
      articles(limit: 2) {
        id
        title
      }
    }
  :response:
    {
      "data": {
        "authors": [
          {
            "id": 1,
            "name": "Justin"
          },
          {
            "id": 2,
            "name": "Beltran"
          }
        ],
        "articles": [
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
