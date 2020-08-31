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

All GraphQL requests for relay queries are made to the Relay GraphQL API.

Endpoint
--------

All requests are ``POST`` requests to the ``/v1beta1/relay`` endpoint.


Request types
-------------

The following types of requests can be made using the Relay GraphQL API:

- :ref:`Query / Subscription <relay_graphql_api_query>`
- :ref:`Mutation <relay_graphql_api_mutation>`

.. _relay_graphql_api_mutation:

Mutation
--------

The Mutation API is similar to :ref:`GraphQL Mutation <graphql_api_mutation>`
except the ``id`` field in table object resolves to Relay Node interface's
``id``.

**Example:**

.. graphiql::
  :view_only:
  :query:
    mutation insert_relay_author {
      insert_author(
        objects: {
          name: "Chris"
          username: "urschris"
        }
      ){
        affected_rows
        returning{
          id
          name
          username
        }
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "affected_rows": 1,
          "returning": [
            {
              "id": "WzEsICJwdWJsaWMiLCAiYXV0aG9yIiwgOF0=",
              "name": "Chris",
              "username": "urschris"
            }
          ]
        }
      }
    }

Batching requests
-----------------

The Relay GraphQL API provides support for batched requests over ``/v1beta1/relay`` endpoint.

**Example:** using a client which supports batching (such as Apollo Client), we can send two
query operations in one request:

.. graphiql::
  :view_only:
  :query:
    query first {
      author_connection(where: {id: {_eq: 1}}){
        edges{
          node{
            id
            name
            username
          }
        }
      }
    }
    query second {
      author_connection(where: {id: {_eq: 2}}){
        edges{
          node{
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
