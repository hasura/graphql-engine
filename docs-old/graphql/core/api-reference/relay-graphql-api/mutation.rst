.. meta::
   :description: Hasura Relay GraphQL API mutations API reference
   :keywords: hasura, docs, GraphQL Relay API, API reference, mutation

.. _relay_graphql_api_mutation:

Relay API Reference - Mutation
==============================

The Mutation API is similar to :ref:`GraphQL mutation <graphql_api_mutation>`
except that the ``id`` field in the table object resolves to Relay Node interface's
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
