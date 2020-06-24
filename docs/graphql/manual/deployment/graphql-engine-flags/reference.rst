.. meta::
   :description: Hasura GraphQL engine server flags reference
   :keywords: hasura, docs, deployment, server, flags

.. _server_flag_reference:

GraphQL engine server flags reference
=====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Every GraphQL engine command is structured as:

.. code-block:: bash

   $ graphql-engine <server-flags> serve <command-flags>

The flags can be passed as ENV variables as well.

Server flags
^^^^^^^^^^^^

For the ``graphql-engine`` command these are the available flags and ENV variables:


.. list-table::
   :header-rows: 1
   :widths: 15 20 30

   * - Flag
     - ENV variable
     - Description

   * - ``--database-url <DB_URL>``
     - ``HASURA_GRAPHQL_DATABASE_URL``
     - Postgres database URL:

       ``postgres://<user>:<password>@<host>:<port>/<db-name>``

       Example: ``postgres://admin:mypass@mydomain.com:5432/mydb``

Or you can specify the following options *(only via flags)*:

.. code-block:: none

      --host                      Postgres server host
  -p, --port                      Postgres server port
  -u, --user                      Database user name
  -p, --password                  Password of the user
  -d, --dbname                    Database name to connect to
  -o, --pg-connection-options     PostgreSQL connection options

.. note::

   The default configuration of PostgreSQL 11 and older may result in loss of
   precision when retrieving IEEE 754 style data, such as ``float4``, ``real``
   or ``double precision`` values, from the database.  To avoid this, set the
   ``extra_float_digits`` parameter to 3.  This can be done by passing
   ``'--pg-connection-options=-c extra_float_digits=3'`` to ``graphql-engine``, or by passing
   this option as part of the database url:

   ``postgres://admin:mypass@mydomain.com:5432/mydb?options=-c%20extra_float_digits%3D3``

.. _command-flags:

Command flags
^^^^^^^^^^^^^

For the ``serve`` sub-command these are the available flags and ENV variables:

.. list-table::
   :header-rows: 1
   :widths: 15 20 30

   * - Flag
     - ENV variable
     - Description

   * - ``--server-port <PORT>``
     - ``HASURA_GRAPHQL_SERVER_PORT``
     - Port on which graphql-engine should be served (default: 8080)

   * - ``--server-host <HOST>``
     - ``HASURA_GRAPHQL_SERVER_HOST``
     - Host on which graphql-engine will listen (default: ``*``)

   * - ``--enable-console <true|false>``
     - ``HASURA_GRAPHQL_ENABLE_CONSOLE``
     - Enable the Hasura Console (served by the server on ``/`` and ``/console``) (default: false)

   * - ``--admin-secret <ADMIN_SECRET_KEY>``
     - ``HASURA_GRAPHQL_ADMIN_SECRET``
     - Admin secret key, required to access this instance. This is mandatory
       when you use webhook or JWT.

   * - ``--auth-hook <WEBHOOK_URL>``
     - ``HASURA_GRAPHQL_AUTH_HOOK``
     - URL of the authorization webhook required to authorize requests.
       See auth webhooks docs for more details.

   * - ``--auth-hook-mode <GET|POST>``
     - ``HASURA_GRAPHQL_AUTH_HOOK_MODE``
     - HTTP method to use for the authorization webhook (default: GET)

   * - ``--jwt-secret <JSON_CONFIG>``
     - ``HASURA_GRAPHQL_JWT_SECRET``
     - A JSON string containing type and the JWK used for verifying (and other
       optional details).
       Example: ``{"type": "HS256", "key": "3bd561c37d214b4496d09049fadc542c"}``.
       See the JWT docs for more details.

   * - ``--unauthorized-role <ROLE>``
     - ``HASURA_GRAPHQL_UNAUTHORIZED_ROLE``
     - Unauthorized role, used when access-key is not sent in access-key only
       mode or the ``Authorization`` header is absent in JWT mode.
       Example: ``anonymous``. Now whenever the "authorization" header is
       absent, the request's role will default to ``anonymous``.

   * - ``--cors-domain <DOMAINS>``
     - ``HASURA_GRAPHQL_CORS_DOMAIN``
     - CSV of list of domains, incuding scheme (http/https) and port, to allow for CORS. Wildcard
       domains are allowed. (See :ref:`configure-cors`)

   * - ``--disable-cors``
     - ``HASURA_GRAPHQL_DISABLE_CORS``
     - Disable CORS. Do not send any CORS headers on any request.

   * - ``--ws-read-cookie <true|false>``
     - ``HASURA_GRAPHQL_WS_READ_COOKIE``
     - Read cookie on WebSocket initial handshake even when CORS is disabled.
       This can be a potential security flaw! Please make sure you know what
       you're doing. This configuration is only applicable when CORS is disabled.
       (default: false)

   * - ``--enable-telemetry <true|false>``
     - ``HASURA_GRAPHQL_ENABLE_TELEMETRY``
     - Enable anonymous telemetry (default: true)

   * - N/A
     - ``HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE``
     - Maximum number of concurrent http workers delivering events at any time (default: 100)

   * - N/A
     - ``HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL``
     - Interval in milliseconds to sleep before trying to fetch events again after a fetch
       returned no events from postgres

   * - ``-s, --stripes <NO_OF_STRIPES>``
     - ``HASURA_GRAPHQL_PG_STRIPES``
     - Number of stripes (distinct sub-pools) to maintain with Postgres (default: 1).
       New connections will be taken from a particular stripe pseudo-randomly.

   * - ``-c, --connections <NO_OF_CONNS>``
     - ``HASURA_GRAPHQL_PG_CONNECTIONS``
     - Maximum number of Postgres connections that can be opened per stripe (default: 50).
       When the maximum is reached we will block until a new connection becomes available,
       even if there is capacity in other stripes.

   * - ``--timeout <SECONDS>``
     - ``HASURA_GRAPHQL_PG_TIMEOUT``
     - Each connection's idle time before it is closed (default: 180 sec)

   * - ``--use-prepared-statements <true|false>``
     - ``HASURA_GRAPHQL_USE_PREPARED_STATEMENTS``
     - Use prepared statements for queries (default: true)

   * - ``-i, --tx-iso <TXISO>``
     - ``HASURA_GRAPHQL_TX_ISOLATION``
     - Transaction isolation. read-committed / repeatable-read / serializable (default: read-commited)

   * - ``--stringify-numeric-types``
     - ``HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES``
     - Stringify certain Postgres numeric types, specifically ``bigint``, ``numeric``, ``decimal`` and
       ``double precision`` as they don't fit into the ``IEEE-754`` spec for JSON encoding-decoding.
       (default: false)

   * - ``--enabled-apis <APIS>``
     - ``HASURA_GRAPHQL_ENABLED_APIS``
     - Comma separated list of APIs (options: ``metadata``, ``graphql``, ``pgdump``) to be enabled.
       (default: ``metadata,graphql,pgdump``)

   * - ``--live-queries-multiplexed-refetch-interval``
     - ``HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_REFETCH_INTERVAL``
     - Updated results (if any) will be sent at most once in this interval (in milliseconds) for live queries
       which can be multiplexed. Default: 1000 (1sec)

   * - ``--live-queries-multiplexed-batch-size``
     - ``HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE``
     - Multiplexed live queries are split into batches of the specified size. Default: 100

   * - ``--enable-allowlist``
     - ``HASURA_GRAPHQL_ENABLE_ALLOWLIST``
     - Restrict queries allowed to be executed by the GraphQL engine to those that are part of the configured
       allow-list. Default: ``false`` *(Available for versions > v1.0.0-beta.1)*

   * - ``--console-assets-dir``
     - ``HASURA_GRAPHQL_CONSOLE_ASSETS_DIR``
     - Set the value to ``/srv/console-assets`` for the console to load assets from the server itself
       instead of CDN *(Available for versions > v1.0.0-beta.1)*

   * - ``--enabled-log-types``
     - ``HASURA_GRAPHQL_ENABLED_LOG_TYPES``
     - Set the enabled log types. This is a comma-separated list of log-types to
       enable. Default: ``startup, http-log, webhook-log, websocket-log``. See
       :ref:`log types <log-types>` for more details.

   * - ``--log-level``
     - ``HASURA_GRAPHQL_LOG_LEVEL``
     - Set the logging level. Default: ``info``. Options: ``debug``, ``info``,
       ``warn``, ``error``.

   * - ``--dev-mode``
     - ``HASURA_GRAPHQL_DEV_MODE``
     - Set dev mode for GraphQL requests; include the ``internal`` key in the errors extensions of the response (if required).

   * - ``--admin-internal-errors``
     - ``HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS``
     - Include the ``internal`` key in the errors extensions of the response for GraphQL requests with the admin role (if required).

.. note::

  When the equivalent flags for environment variables are used, the flags will take precedence.
