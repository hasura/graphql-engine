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

Scheme + host with optional wildcard + optional port has to be mentioned.

The default (if the flag or env var is not specified) is ``*``. Which means CORS
headers are sent for all domains.

.. code-block:: bash

    # Accepts from https://app.foo.bar.com , https://api.foo.bar.com etc.
    HASURA_GRAPHQL_CORS_DOMAIN="https://*.foo.bar.com"

    # Accepts from https://app.foo.bar.com:8080 , http://api.foo.bar.com:8080,
    # http://app.localhost, http://api.localhost, http://localhost:3000,
    # http://example.com etc.
    HASURA_GRAPHQL_CORS_DOMAIN="https://*.foo.bar.com:8080, http://*.localhost, http://localhost:3000, http://example.com"

    # Accepts from all domain
    HASURA_GRAPHQL_CORS_DOMAIN="*"

    # Accepts only from http://example.com
    HASURA_GRAPHQL_CORS_DOMAIN="http://example.com"


.. note::

  Top-level domains are not considered as part of wildcard domains. You
  have to add them separately. E.g - ``https://*.foo.com`` doesn't include
  ``https://foo.com``.
