Hasura GraphQL engine server logs (Heroku)
==========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can use the `Heroku CLI <https://devcenter.heroku.com/articles/heroku-cli>`_ to check logs
of Hasura GraphQL engine deployed on Heroku:

.. code-block:: bash

   $ heroku logs --app <hasura-graphql-engine-app-name>

   2018-10-09T11:18:21.306000+00:00 app[web.1]: {"timestamp":"2018-10-09T11:18:21.305+0000", "level":"info", "type":"http-log", "detail":{"status":200, "query_hash":"48c74f902b53a886f9ddc1b7dd12a4a6020d70c3", "http_version":"HTTP/1.1", "query_execution_time":9.477913e-3, "request_id":"b7bb6fb3-97b3-4c6f-a54a-1e0f71a190e9", "url":"/v1/graphql", "user":{"x-hasura-role":"admin"}, "ip":"171.61.77.16", "response_size":15290, "method":"POST", "detail":null}}
   ...

See https://devcenter.heroku.com/articles/logging for more details on logging in Heroku.
