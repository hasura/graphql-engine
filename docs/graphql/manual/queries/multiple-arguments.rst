.. meta::
   :description: Use multiple arguments in a query in Hasura
   :keywords: hasura, docs, query, multiple arguments

.. _multiple_arguments_query:

Using multiple arguments in a query
===================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Multiple arguments can be used together in the same query.

For example, you can use the ``where`` argument to filter the results and then use the ``order_by`` argument to
sort them.

**For example**, fetch a list of authors and only 2 of their published articles that are sorted by their date
of publication:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          author {
            id
            name
            articles(
              where: {is_published: {_eq: true}},
              order_by: {published_on: desc},
              limit: 2
            ) {
              id
              title
              is_published
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
                "name": "Justin",
                "articles": [
                  {
                    "is_published": true,
                    "id": 16,
                    "title": "sem duis aliquam",
                    "published_on": "2018-02-14"
                  },
                  {
                    "is_published": true,
                    "id": 15,
                    "title": "vel dapibus at",
                    "published_on": "2018-01-02"
                  }
                ]
              },
              {
                "id": 2,
                "name": "Beltran",
                "articles": [
                  {
                    "is_published": true,
                    "id": 2,
                    "title": "a nibh",
                    "published_on": "2018-06-10"
                  },
                  {
                    "is_published": true,
                    "id": 9,
                    "title": "sit amet",
                    "published_on": "2017-05-16"
                  }
                ]
              },
              {
                "id": 3,
                "name": "Sidney",
                "articles": [
                  {
                    "is_published": true,
                    "id": 6,
                    "title": "sapien ut",
                    "published_on": "2018-01-08"
                  },
                  {
                    "is_published": true,
                    "id": 11,
                    "title": "turpis eget",
                    "published_on": "2017-04-14"
                  }
                ]
              },
              {
                "id": 4,
                "name": "Anjela",
                "articles": [
                  {
                    "is_published": true,
                    "id": 1,
                    "title": "sit amet",
                    "published_on": "2017-08-09"
                  },
                  {
                    "is_published": true,
                    "id": 3,
                    "title": "amet justo morbi",
                    "published_on": "2017-05-26"
                  }
                ]
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
        "query": "query { author { id name articles(where: { is_published: { _eq: true }}, order_by: { published_on: desc }, limit: 2) { id title is_published published_on }}}"
      }
