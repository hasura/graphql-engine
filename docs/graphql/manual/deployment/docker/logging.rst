Hasura GraphQL engine server logs (Docker)
==========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can check logs of Hasura GraphQL engine deployed using Docker by checking the logs of the
GraphQL engine container:

.. code-block:: bash

  $ docker ps

  CONTAINER ID        IMAGE                       ...
  cdfbc6b94c70        hasura/graphql-engine..     ...

  $ docker logs cdfbc6b94c70

  {"timestamp":"2018-10-09T11:20:32.054+0000", "level":"info", "type":"http-log", "detail":{"status":200, "query_hash":"01640c6dd131826cff44308111ed40d7fbd1cbed", "http_version":"HTTP/1.1", "query_execution_time":3.0177627e-2, "request_id":null, "url":"/v1/graphql", "user":{"x-hasura-role":"admin"}, "ip":"127.0.0.1", "response_size":209329, "method":"POST", "detail":null}}
  ...


See https://docs.docker.com/config/containers/logging for more details on logging in Docker.
