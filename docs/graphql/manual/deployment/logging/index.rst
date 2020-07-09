.. meta::
   :description:  Hasura GraphQL engine server logs
   :keywords: hasura, docs, deployment, logs, server logs

.. _hge_logs:

Hasura GraphQL engine logs
==========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Accessing logs
--------------

Based on your deployment method, the Hasura GraphQL engine logs can be accessed as follows:

- :ref:`On Heroku <heroku_logs>`
- :ref:`On Docker <docker_logs>`
- :ref:`On Kubernetes <kubernetes_logs>`

.. _log-types:

Different log-types
-------------------

The Hasura GraphQL engine has different kind of log-types depending on the sub-system or the layer. A log-type is simply the ``type`` field in a log line, which indicates which sub-system the log comes from.

For example, the HTTP webserver logs incoming requests as an access log and is called ``http-log``. Similarly logs from the websocket layer are called ``websocket-log``, logs from the event trigger system are called ``event-trigger`` etc.


You can configure the GraphQL engine to enable/disable certain log-types using the the ``--enabled-log-types`` flag or the ``HASURA_GRAPHQL_ENABLED_LOG_TYPES`` env var. See :ref:`server_flag_reference`

Default enabled log-types are: ``startup, http-log, webhook-log, websocket-log``

All the log-types that can be enabled/disabled are:

.. list-table:: Configurable log-types
   :header-rows: 1
   :widths: 10 25 10

   * - Log type
     - Description
     - Log Level

   * - ``startup``
     - Information that is logged during startup
     - ``info``

   * - ``query-log``
     - Logs: the entire GraphQL query with variables, generated SQL statements
       (only for queries, not for mutations/subscriptions or remote schema
       queries), the operation name (if provided in the GraphQL request)
     - ``info``

   * - ``http-log``
     - Http access and error logs at the webserver layer (handling GraphQL and metadata requests)
     - ``info`` and ``error``

   * - ``websocket-log``
     - Websocket events and error logs at the websocket server layer (handling GraphQL requests)
     - ``info`` and ``error``

   * - ``webhook-log``
     - Logs responses and errors from the authorization webhook (if setup)
     - ``info`` and ``error``


Apart from the above, there are other internal log-types which cannot be configured:

.. list-table:: Internal log-types
   :header-rows: 1
   :widths: 10 25 10

   * - Log type
     - Description
     - Log Level

   * - ``pg-client``
     - Logs from the postgres client library
     - ``warn``

   * - ``metadata``
     - Logs inconsistent metadata items
     - ``warn``

   * - ``jwk-refresh-log``
     - Logs information and errors about periodic refreshing of JWK
     - ``info`` and ``error``

   * - ``telemetry-log``
     - Logs error (if any) while sending out telemetry data
     - ``info``

   * - ``event-trigger``
     - Logs HTTP responses from the webhook, HTTP exceptions and internal
       errors
     - ``info`` and ``error``

   * - ``ws-server``
     - Debug logs from the websocket server, mostly used internally for debugging
     - ``debug``

   * - ``schema-sync-thread``
     - Logs internal events, when it detects schema has changed on Postgres and
       when it reloads the schema
     - ``info`` and ``error``

Logging levels
--------------

You can set the desired logging level on the server using the ``log-level`` flag or the ``HASURA_GRAPHQL_LOG_LEVEL`` env var. See :ref:`server_flag_reference`.

The default log-level is ``info``.

Setting a log-level will print all logs of priority greater than the set level. The log-level hierarchy is: ``debug > info > warn > error``

For example, setting ``--log-level=warn``, will enable all warn and error level logs only. So even if you have enabled ``query-log`` it won't be printed as the level of ``query-log`` is ``info``.

See :ref:`log-types <log-types>` for more details on log-level of each log-type.

Log structure and metrics
-------------------------

All requests are identified by a request id. If the client sends a ``x-request-id`` header then that is used, otherwise a request id is generated for each request. This is also sent back to the client as a response header (``x-request-id``). This is useful to correlate logs from the server and the client.

**query-log** structure
^^^^^^^^^^^^^^^^^^^^^^^

On enabling verbose logging, i.e. enabling ``query-log``,
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


**http-log** structure
^^^^^^^^^^^^^^^^^^^^^^

This is how the HTTP access logs look like:

- On success response:

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

The ``type`` in the log will be ``http-log`` for HTTP access/error log. This
log contains basic information about the HTTP request and the GraphQL operation.

It has two important "keys" under the ``detail`` section - ``operation`` and ``http_info``.

``http_info`` lists various information regarding the HTTP request, e.g. IP
address, URL path, HTTP status code etc.

``operation`` lists various information regarding the GraphQL query/operation.

- ``query_execution_time``: the time taken to parse the GraphQL query (from JSON
  request), compile it to SQL with permissions and user session variables, and
  then executing it and fetching the results back from Postgres. The unit is in
  seconds.

- ``user_vars``: contains the user session variables. Or the ``x-hasura-*``
  session variables inferred from the authorization mode.

- ``request_id``: A unique ID for each request. If the client sends a
  ``x-request-id`` header then that is respected, otherwise a UUID is generated
  for each request.

- ``response_size``: Size of the response in bytes.

- ``error``: *optional*. Will contain the error object when there is an error,
  otherwise this will be ``null``. This key can be used to detect if there is an
  error in the request. The status code for error requests will be ``200`` on
  the ``v1/graphql`` endpoint.

- ``query``: *optional*. This will contain the GraphQL query object only when
  there is an error. On successful response this will be ``null``.

**websocket-log** structure
^^^^^^^^^^^^^^^^^^^^^^^^^^^
This is how the Websocket logs look like:

- On successful operation start:

.. code-block:: json

    {
      "timestamp": "2019-06-10T10:52:54.247+0530",
      "level": "info",
      "type": "websocket-log",
      "detail": {
        "event": {
          "type": "operation",
          "detail": {
            "request_id": "d2ede87d-5cb7-44b6-8736-1d898117722a",
            "operation_id": "1",
            "query": {
              "variables": {},
              "query": "subscription {\n  author {\n    name\n  }\n}\n"
            },
            "operation_type": {
              "type": "started"
            },
            "operation_name": null
          }
        },
        "connection_info": {
          "websocket_id": "f590dd18-75db-4602-8693-8150239df7f7",
          "jwt_expiry": null,
          "msg": null
        },
        "user_vars": {
          "x-hasura-role": "admin"
        }
      }
    }

- On operation stop:

.. code-block:: json

    {
      "timestamp": "2019-06-10T11:01:40.939+0530",
      "level": "info",
      "type": "websocket-log",
      "detail": {
        "event": {
          "type": "operation",
          "detail": {
            "request_id": null,
            "operation_id": "1",
            "query": null,
            "operation_type": {
              "type": "stopped"
            },
            "operation_name": null
          }
        },
        "connection_info": {
          "websocket_id": "7f782190-fd58-4305-a83f-8e17177b204e",
          "jwt_expiry": null,
          "msg": null
        },
        "user_vars": {
          "x-hasura-role": "admin"
        }
      }
    }

- On error:

.. code-block:: json

    {
      "timestamp": "2019-06-10T10:55:20.650+0530",
      "level": "info",
      "type": "websocket-log",
      "detail": {
        "event": {
          "type": "operation",
          "detail": {
            "request_id": "150e3e6a-e1a7-46ba-a9d4-da6b192a4005",
            "operation_id": "1",
            "query": {
              "variables": {},
              "query": "subscription {\n  author {\n    namex\n  }\n}\n"
            },
            "operation_type": {
              "type": "query_err",
              "detail": {
                "path": "$.selectionSet.author.selectionSet.namex",
                "error": "field \"namex\" not found in type: 'author'",
                "code": "validation-failed"
              }
            },
            "operation_name": null
          }
        },
        "connection_info": {
          "websocket_id": "49932ddf-e54d-42c6-bffb-8a57a1c6dcbe",
          "jwt_expiry": null,
          "msg": null
        },
        "user_vars": {
          "x-hasura-role": "admin"
        }
      }
    }

Monitoring frameworks
---------------------

You can integrate the logs emitted by Hasura GraphQL with external monitoring tools for better visibility as per
your convenience.

For some examples, see :ref:`guides_monitoring_frameworks`.

.. toctree::
   :titlesonly:
   :hidden:

   Heroku <heroku>
   Docker <docker>
   Kubernetes <kubernetes>
