.. meta::
   :description: Hasura Relay GraphQL API reference
   :keywords: hasura, docs, GraphQL API, reference, relay

.. _api_reference_relay_graphql:

Relay GraphQL API Reference
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

All GraphQL requests for Relay queries are made to the Relay GraphQL API.

Endpoint
--------

All requests are ``POST`` requests to the ``/v1beta1/relay`` endpoint.


Request types
-------------

The following types of requests can be made using the Relay GraphQL API:

- :ref:`Query / Subscription <relay_graphql_api_query>`
- :ref:`Mutation <relay_graphql_api_mutation>`


Batching requests
-----------------

The Relay GraphQL API provides support for batched requests over the ``/v1beta1/relay`` endpoint.

**Example:** using a client which supports batching (such as Apollo Client), we can send two
query operations in one request:

.. graphiql::
  :view_only:
  :query:
    query first {
      author_connection(where: {id: {_eq: 1}}){
        edges {
          node {
            id
            name
            username
          }
        }
      }
    }
    query second {
      author_connection(where: {id: {_eq: 2}}){
        edges {
          node {
            id
            name
            username
          }
        }
      }
    }
  :response:
    [
      {
        "data": {
          "author_connection": {
            "edges": [
              {
                "node": {
                  "id": "WzEsIHB1YmxpYywgYXV0aG9yLCAxXQo=",
                  "name": "Chris",
                  "username": "urschris"
                }
              }
            ]
          }
        }
      },
      {
        "data": {
          "author_connection": {
            "edges": [
              {
                "node": {
                  "id": "WzEsIHB1YmxpYywgYXV0aG9yLCAyXQo=",
                  "name": "Blake",
                  "username": "blake99"
                }
              }
            ]
          }
        }
      }
    ]

.. toctree::
  :maxdepth: 1
  :hidden:

  Query / Subscription <query>
  Mutation <mutation>
