.. meta::
   :description: Hasura GraphQL API reference
   :keywords: hasura, docs, GraphQL API, reference

.. _api_reference_graphql:

GraphQL API Reference
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

All GraphQL requests for queries, subscriptions and mutations are made to the GraphQL API.

Endpoint
--------

All requests are ``POST`` requests to the ``/v1/graphql`` (or ``/v1alpha1/graphql``) endpoint.

.. note::

   The ``/v1/graphql`` endpoint returns HTTP 200 status codes for all responses.
   This is a **breaking** change from the ``/v1alpha1/graphql`` behaviour, where
   request errors and internal errors were responded with 4xx and 5xx status
   codes.

Request types
-------------

The following types of requests can be made using the GraphQL API:

- :ref:`Query / Subscription <graphql_api_query>`
- :ref:`Mutation <graphql_api_mutation>`

Batching operations
-------------------

The GraphQL API provides support for batched operations (which can be a combination of queries and mutations).
The endpoint will accept an array of operations in place of a single operation, and return an array of corresponding 
responses.

**Example:** using a client which supports batching (such as Apollo Client), we can send two
query operations in one request:

.. graphiql::
  :view_only:
  :query:
    query first {
      author(where: {id: {_eq: 1}}) {
        id
        name
      }
    }
    query second {
      author(where: {id: {_eq: 2}}) {
        id
        name
      }
    }
  :response:
    [
      {
        "data": {
          "author": [
            {
              "id": 1,
              "name": "Justin"
            }
          ]
        }
      },
      {
        "data": {
          "author": [
            {
              "id": 2,
              "name": "Beltran"
            }
          ]
        }
      }
    ]


.. toctree::
  :maxdepth: 1
  :hidden:

  Query / Subscription <query>
  Mutation <mutation>
