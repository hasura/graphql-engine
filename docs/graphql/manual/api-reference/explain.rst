.. meta::
   :description: Hasura explain API reference
   :keywords: hasura, docs, explain API, API reference

.. _explain_api_reference:

Explain API Reference
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Explain API is used to analyse queries and subscriptions. It returns a list of Postgres plans for a query and a single Postgres plan for a subscription, based
on the defined permissions.

Endpoint
--------

All requests are ``POST`` requests to the ``/v1/graphql/explain`` endpoint.

API Spec
--------

Request
^^^^^^^

The request expects a JSON object with the following keys:
  - ``query``: the GraphQL query to be analysed
  - ``user`` (optional): the ``x-hasura-role`` along with session variables

.. code-block:: http

   POST /v1/graphql/explain HTTP/1.1
   Content-Type: application/json

   {
        "query": "<query>",
        "user": {
            "x-hasura-role" : "...",
            "x-hasura-session-var1" : "..."
        }
   }

Sample request
**************

.. code-block:: http

   POST /v1/graphql/explain HTTP/1.1
   Content-Type: application/json

   {
        "query": {
            "query": "query getAuthors { authors { name }}",
            "operationName": "getAuthors"
        }
   }


Response
^^^^^^^^

The response for a query is a list of plans:

.. code-block:: none

    [
        {
            "field": String -- "name of the field",
            "sql": String -- "the generated SQL for the operation",
            "plan": [String] -- "Postgres's plan for the generated SQL"
        }
    ]

The response for a subscription is a single plan:

.. code-block:: none

    {
        "sql": String -- "the generated SQL for the operation",
        "plan": [String] -- "Postgres's plan for the generated SQL"
    }

Sample response for a query
***************************

.. code-block:: json

    [
        {
            "field": "authors",
            "sql": "SELECT  coalesce(json_agg(\"root\" ), '[]' ) AS \"root\" FROM  (SELECT  row_to_json((SELECT  \"_1_e\"  FROM  (SELECT  \"_0_root.base\".\"name\" AS \"name\"       ) AS \"_1_e\"      ) ) AS \"root\" FROM  (SELECT  *  FROM \"public\".\"authors\"  WHERE ('true')     ) AS \"_0_root.base\"      ) AS \"_2_root\"      ",
            "plan": [
                "Aggregate  (cost=14.27..14.27 rows=1 width=32)",
                "  ->  Seq Scan on authors  (cost=0.00..11.83 rows=610 width=32)",
                "  SubPlan 1",
                "    ->  Result  (cost=0.00..0.00 rows=1 width=32)"
            ]
        }
    ]

Sample response for a subscription
**********************************

.. code-block:: json

    {
        "sql": "\n        select\n          _subs.result_id, _fld_resp.root as result\n          from\n            unnest(\n              $1::uuid[], $2::json[]\n            ) _subs (result_id, result_vars)\n          left outer join lateral\n            (\n        SELECT  coalesce(json_agg(\"root\" ), '[]' ) AS \"root\" FROM  (SELECT  row_to_json((SELECT  \"_1_e\"  FROM  (SELECT  \"_0_root.base\".\"name\" AS \"name\"       ) AS \"_1_e\"      ) ) AS \"root\" FROM  (SELECT  *  FROM \"public\".\"authors\"  WHERE ('true')     ) AS \"_0_root.base\"      ) AS \"_2_root\"      \n            ) _fld_resp ON ('true')\n        ",
        "plan": [
            "Nested Loop Left Join  (cost=14.27..14.93 rows=100 width=48)",
            "  ->  Function Scan on _subs  (cost=0.00..0.30 rows=100 width=16)",
            "  ->  Materialize  (cost=14.27..14.28 rows=1 width=32)",
            "        ->  Aggregate  (cost=14.27..14.27 rows=1 width=32)",
            "              ->  Seq Scan on authors  (cost=0.00..11.83 rows=610 width=32)",
            "              SubPlan 1",
            "                ->  Result  (cost=0.00..0.00 rows=1 width=32)"
        ]
    }

Disabling Explain API
---------------------

The Explain API is part of the :ref:`Metadata API <metadata_apis>` and can only be disabled by disabling the same.
