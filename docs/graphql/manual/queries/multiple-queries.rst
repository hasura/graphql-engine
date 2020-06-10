.. meta::
   :description: Run multiple queries in a request in Hasura
   :keywords: hasura, docs, query, multiple queries, request

.. _multiple_queries:

Multiple queries in a request
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Execution
---------

If multiple queries are part of the same request, **they are executed in parallel**, the individual responses are
collated and returned. You can fetch objects of different unrelated types in the same query.

Run multiple top level queries in the same request
--------------------------------------------------

**For example**, fetch a list of ``authors`` and a list of ``articles``:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
          "query": "query { author(limit: 2) { id name } article(limit: 2) { id title }}"
      }
