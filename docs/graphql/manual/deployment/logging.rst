Hasura GraphQL engine logs
==========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Accessing logs
--------------

Based on your deployment method, Hasura GraphQL engine logs can be accessed as follows:

- :doc:`On Heroku <heroku/logging>`
- :doc:`On Docker <docker/logging>`
- :doc:`On Kubernetes <kubernetes/logging>`

Log structure and metrics
-------------------------

Query log structure
^^^^^^^^^^^^^^^^^^^

On enabling :doc:`verbose logging <../deployment/graphql-engine-flags/reference>`,
GraphQL engine will log the full GraphQL query object on each request.

It will also log the generated SQL for GraphQL queries (but not mutations and
subscriptions).

.. code-block:: json

    {
      "timestamp": "2019-06-03T13:25:10.915+0530",
      "level": "info",
      "type": "query-log",
      "detail": {
        "request_id": "840f952d-c489-4d21-a87a-cc23ad17926a",
        "query": {
          "variables": {
            "limit": 10
          },
          "operationName": "getProfile",
          "query": "query getProfile($limit: Int!) {\n  profile(limit: $limit, where: {username: {_like: \"%a%\"}}) {\n    username\n  }\n  myusername: profile (where: {username: {_eq: \"foobar\"}}) {\n    username\n  }\n}\n"
        },
        "generated_sql": {
          "profile": {
            "prepared_arguments": ["{\"x-hasura-role\":\"admin\"}", "%a%"],
            "query": "SELECT  coalesce(json_agg(\"root\" ), '[]' ) AS \"root\" FROM  (SELECT  row_to_json((SELECT  \"_1_e\"  FROM  (SELECT  \"_0_root.base\".\"username\" AS \"username\"       ) AS \"_1_e\"      ) ) AS \"root\" FROM  (SELECT  *  FROM \"public\".\"profile\"  WHERE ((\"public\".\"profile\".\"username\") LIKE ($2))     ) AS \"_0_root.base\"     LIMIT 10 ) AS \"_2_root\"      "
          },
          "myusername": {
            "prepared_arguments": ["{\"x-hasura-role\":\"admin\"}", "foobar"],
            "query": "SELECT  coalesce(json_agg(\"root\" ), '[]' ) AS \"root\" FROM  (SELECT  row_to_json((SELECT  \"_1_e\"  FROM  (SELECT  \"_0_root.base\".\"username\" AS \"username\"       ) AS \"_1_e\"      ) ) AS \"root\" FROM  (SELECT  *  FROM \"public\".\"profile\"  WHERE ((\"public\".\"profile\".\"username\") = ($2))     ) AS \"_0_root.base\"      ) AS \"_2_root\"      "
          }
        }
      }
    }


The ``type`` of in the log with be ``query-log``. All the details are nested
under the ``detail`` key.

This log contains 3 important fields:

- ``request_id``: A unique ID for each request. If the client sends a
  ``x-request-id`` header then that is respected, otherwise a UUID is generated
  for each request. This is useful to correlate between ``http-log`` and
  ``query-log``.

- ``query``: Contains the full GraphQL request including the variables and
  operation name.

- ``generated_sql``: this contains the generated SQL for GraphQL queries. For
  mutations and subscriptions this field will be ``null``.


HTTP access log structure
^^^^^^^^^^^^^^^^^^^^^^^^^

This is how the HTTP access log look like:

- On successful response:

.. code-block:: json

    {
      "timestamp": "2019-05-30T23:40:24.654+0530",
      "level": "info",
      "type": "http-log",
      "detail": {
        "operation": {
          "query_execution_time": 0.009240042,
          "user_vars": {
            "x-hasura-role": "user"
          },
          "error": null,
          "request_id": "072b3617-6653-4fd5-b5ee-580e9d098c3d",
          "response_size": 105,
          "query": null
        },
        "http_info": {
          "status": 200,
          "http_version": "HTTP/1.1",
          "url": "/v1/graphql",
          "ip": "127.0.0.1",
          "method": "POST"
        }
      }
    }


- On error response:

.. code-block:: json

    {
      "timestamp": "2019-05-29T15:22:37.834+0530",
      "level": "info",
      "type": "http-log",
      "detail": {
        "operation": {
          "query_execution_time": 0.000656144,
          "user_vars": {
            "x-hasura-role": "user",
            "x-hasura-user-id": "1"
          },
          "error": {
            "path": "$.selectionSet.profile.selectionSet.usernamex",
            "error": "field \"usernamex\" not found in type: 'profile'",
            "code": "validation-failed"
          },
          "request_id": "072b3617-6653-4fd5-b5ee-580e9d098c3d",
          "response_size": 142,
          "query": {
            "variables": {
              "limit": 10
            },
            "operationName": "getProfile",
            "query": "query getProfile($limit: Int!) { profile(limit: $limit, where:{username: {_like: \"%a%\"}}) { usernamex} }"
          }
        },
        "http_info": {
          "status": 200,
          "http_version": "HTTP/1.1",
          "url": "/v1/graphql",
          "ip": "127.0.0.1",
          "method": "POST"
        }

    }


Breakdown of the log
++++++++++++++++++++

The ``type`` in the log will be ``http-log`` for HTTP access/error log. This
log contains basic information about the HTTP request and the GraphQL operation.

It has two important "keys" under the ``detail`` section - ``operation`` and ``http_info``.

``http_info`` lists various information regarding the HTTP request, e.g. IP
address, URL path, HTTP status code etc.

``operation`` lists various information regarding the GraphQL query/operation.

- ``query_execution_time``: the time taken to parse the GraphQL query, compile
  it to SQL with permissions and user session variables, and then executing it
  and fetching the results back from Postgres. The unit is in seconds.

- ``user_vars``: contains the user session variables. Or the ``x-hasura-*``
  session variables inferred from the authorization mode.

- ``request_id``: A unique ID for each request. If the client sends a
  ``x-request-id`` header then that is respected, otherwise a UUID is generated
  for each request.

- ``response_size``: Size of the response in bytes.

- ``error``: Is optional. Will contain the error object when there is an error,
  otherwise this will be ``null``. This key can be used to detect if there is an
  error in the request. The status code for error requests will be ``200`` on
  the ``v1/graphql`` endpoint.

- ``query``: Optional. This will contain the GraphQL query object only when
  there is an error. On successful response this will be ``null``.


Monitoring frameworks
---------------------

You can integrate the logs emitted by Hasura GraphQL with external monitoring tools for better visibility as per
your convenience.

For some examples, see :doc:`../guides/monitoring/index`
