GraphQL engine server flags reference
=====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Every GraphQL engine command is structured as:

.. code-block:: bash

   $ graphql-engine <server-flags> serve <command-flags>

Server flags
^^^^^^^^^^^^

For ``graphql-engine`` command these are the flags available


.. list-table::
   :header-rows: 1
   :widths: 10 10 50

   * - Flag
     - Environment variable
     - Description

   * - ``--database-url``
     - ``HASURA_GRAPHQL_DATABASE_URL``
     - Postgres database URL
       ``postgres://<user>:<password>@<host>:<port>/<db-name>``
       Example: ``postgres://admin:mypass@mydomain.com:5432/mydb``

Or either you can specify following options (only via flags)

.. code-block:: none

      --host               Postgres server host
  -p, --port               Postgres server port
  -u, --user               Database user name
  -p, --password           Password of the user
  -d, --dbname             Database name to connect to


Command flags
^^^^^^^^^^^^^

For ``serve`` subcommand these are the flags and environment variables available

.. list-table::
   :header-rows: 1
   :widths: 10 20 40

   * - Flag
     - Environment variable
     - Description

   * - ``--server-port <PORT>``
     - ``HASURA_GRAPHQL_SERVER_PORT``
     - Port on which graphql-engine should be served (default: 8080)

   * - ``--server-host <HOST>``
     - ``HASURA_GRAPHQL_SERVER_HOST``
     - Host on which graphql-engine will listen (default: ``*``)

   * - ``--enable-console``
     - ``HASURA_GRAPHQL_ENABLE_CONSOLE``
     - Enable the Hasura Console (served by the server on ``/`` and ``/console``)

   * - ``--access-key <SECRET ACCESS KEY>``
     - ``HASURA_GRAPHQL_ACCESS_KEY``
     - Secret access key, for admin access to this instance. This is mandatory
       when you use webhook or JWT.

   * - ``--auth-hook <WEB HOOK URL>``
     - ``HASURA_GRAPHQL_AUTH_HOOK``
     - URL of the authorization webhook required to authorize requests. See auth
       webhooks in docs.

   * - ``--auth-hook-mode <GET|POST>``
     - ``HASURA_GRAPHQL_AUTH_HOOK_MODE``
     - HTTP method to use for the authorization webhook (default: GET)

   * - ``--jwt-secret <JSON CONFIG>``
     - ``HASURA_GRAPHQL_JWT_SECRET``
     - A JSON string containing type and the JWK used for verifying (and other
       optional details). E.g - ``{"type": "HS256", "key":
       "3bd561c37d214b4496d09049fadc542c"}``. See the JWT docs for more details.

   * - ``--unauthorized-role <ROLE>``
     - ``HASURA_GRAPHQL_UNAUTHORIZED_ROLE``
     - Unauthorized role, used when access-key is not sent in access-key only
       mode or "Authorization" header is absent in JWT mode. Example:
       ``--unauthorized-role anonymous``. Now whenever "Authorization" header is
       absent, request's role will default to "anonymous".

   * - ``--cors-domain <DOMAINS>``
     - ``HASURA_GRAPHQL_CORS_DOMAIN``
     - CSV of list of domains, excluding scheme (http/https) and including port,
       to allow CORS for. Wildcard domains are allowed. See examples below for
       valid configurations.

   * - ``--disable-cors``
     - N/A
     - Disable CORS. Do not send any CORS headers on any request.

   * - ``--enable-telemetry <true|false>``
     - ``HASURA_GRAPHQL_ENABLE_TELEMETRY``
     - Enable anonymous telemetry (default: true)

   * - N/A
     - ``HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE``
     - Max event threads

   * - N/A
     - ``HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL``
     - Postgres events polling interval

   * - ``-s,--stripes <NO OF STRIPES>``
     - ``HASURA_GRAPHQL_PG_STRIPES``
     - Number of conns that need to be opened to Postgres (default: 1)

   * - ``-c,--connections <NO OF CONNS>``
     - ``HASURA_GRAPHQL_PG_CONNECTIONS``
     - Number of conns that need to be opened to Postgres (default: 50)

   * - ``--timeout <SECONDS>``
     - ``HASURA_GRAPHQL_PG_TIMEOUT``
     - Each connection's idle time before it is closed (default: 180 sec)

   * - ``--use-prepared-statements <true|false>``
     - ``HASURA_GRAPHQL_USE_PREPARED_STATEMENTS``
     - Use prepared statements for queries (default: true)

   * - ``-i,--tx-iso <TXISO>``
     - ``HASURA_GRAPHQL_TX_ISOLATION``
     - transaction isolation. read-committed / repeatable-read / serializable (default: read-commited)


CORS configuration examples
^^^^^^^^^^^^^^^^^^^^^^^^^^^

No scheme (i.e., http/https) is required. Port has to be mentioned. Hasura will
match both http/https schemes.

.. code-block:: bash

    # Accepts from https://app.foo.bar.com , http://api.foo.bar.com etc.
    HASURA_GRAPHQL_CORS_DOMAIN="*.foo.bar.com"

    # Accepts from https://app.foo.bar.com:8080 , http://api.foo.bar.com:8080,
    # http://app.localhost, http://api.localhost, http://localhost:3000,
    # http://example.com etc.
    HASURA_GRAPHQL_CORS_DOMAIN="*.foo.bar.com:8080, *.localhost, localhost:3000, example.com"

    # Accepts from all domain
    HASURA_GRAPHQL_CORS_DOMAIN="*"

    # Accepts only from example.com
    HASURA_GRAPHQL_CORS_DOMAIN="example.com"


..
    For ``serve`` subcommand these are the flags available

    .. code-block:: none

          --server-port        Port on which graphql-engine should be served (default: 8080)
          --access-key         Secret access key, required to access this instance.
                                If specified client needs to send 'X-Hasura-Access-Key'
                                header
          --cors-domain        The domain, including sheme and port, to allow CORS for
          --disable-cors       Disable CORS handling
          --auth-hook          The authentication webhook, required to authenticate
                                incoming request
          --jwt-secret         The JSON containing type and the JWK used for
                                verifying. e.g: `{"type": "HS256", "key":
                              "<your-hmac-shared-secret>"}`,`{"type": "RS256",
                              "key": "<your-PEM-RSA-public-key>"}
      -s, --stripes            Number of stripes
      -c, --connections        Number of connections that need to be opened to Postgres
          --timeout            Each connection's idle time before it is closed
      -i, --tx-iso             Transaction isolation. read-commited / repeatable-read /
                                serializable
          --root-dir           This static dir is served at / and takes precedence over
                                all routes
          --enable-console     Enable API console. It is served at '/' and '/console'
