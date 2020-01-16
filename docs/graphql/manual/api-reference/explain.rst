.. _explain_api_reference:

Explain API Reference
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Health API is a public endpoint which gives info on the server health.
This Explain API returns a list of Postgres plans for a query or subscription. The plans
will be different for each role, based on the defined permissions.

Endpoint
--------

All requests are ``POST`` requests to the ``/v1/graphql/explain`` endpoint.

API Spec
--------

Request
^^^^^^^

The request expects a JSON object with the following keys:
  - `query`: the GraphQL query which needs to be analyzed
  - `user` (optional): session variables along with 'x-hasura-role'

.. code-block:: http

   POST /v1/graphql/explain HTTP/1.1
   Content-Type: application/json

   {
        "query": { table { field } },
        "user": {
            "x-hasura-role" : "...",
            "x-hasura-session-var1" : "..."
        }
   }

Response
^^^^^^^^

The response is a list of plans for a query or a single plan for a subscription. The
structure of each plan is as follows:

.. code-block:: http

    {
        "field": String -- "name of the field",
        "sql": String -- "the generated SQL for the operation",
        "plan": [String] -- "Postgres's plan for the generated SQL"
    }

Sample response
***************

.. code-block:: http

   {
       {
            "field": String -- "name of the field",
            "sql": String -- "the generated SQL for the operation",
            "plan": [String] -- "Postgres's plan for the generated SQL"
        },
        {
            "field": String -- "name of the field",
            "sql": String -- "the generated SQL for the operation",
            "plan": [String] -- "Postgres's plan for the generated SQL"
        },
        {
            "field": String -- "name of the field",
            "sql": String -- "the generated SQL for the operation",
            "plan": [String] -- "Postgres's plan for the generated SQL"
        }
   }


Disabling Explain API
---------------------

The ``explain`` API endpoint is public and cannot be disabled.
