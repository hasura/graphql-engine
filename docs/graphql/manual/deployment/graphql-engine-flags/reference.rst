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

.. code-block:: none

      --database-url       Postgres database URL
                           <postgres/postgresql>://<user>:<password>@<host>:<port>/<db-name>
                           Example: postgres://admin:mypass@mydomain.com:5432/mydb

Or either you can specify following options

.. code-block:: none

      --host               Postgres server host
  -p, --port               Postgres server port
  -u, --user               Database user name
  -p, --password           Password of the user
  -d, --dbname             Database name to connect to

Command flags
^^^^^^^^^^^^^

For ``serve`` subcommand these are the flags available

.. code-block:: none

       --server-port            Port on which graphql-engine should be served (default: 8080)

       --access-key             Secret access key, required to access this instance.
                                If specified client needs to send 'X-Hasura-Access-Key'
                                header

       --cors-domain            The domain, including sheme and port, to allow CORS for

       --disable-cors           Disable CORS handling

       --auth-hook              The authentication webhook, required to authenticate
                                incoming request

       --auth-hook-mode         The authentication webhook mode. GET|POST (default: GET)

       --jwt-secret             The JSON containing type and the JWK used for
                                verifying. e.g: `{"type": "HS256", "key":
                               "<your-hmac-shared-secret>"}`,`{"type": "RS256",
                               "key": "<your-PEM-RSA-public-key>"}

       --unauthorized-role      Unauthorized role, used when access-key is not sent in
                                access-key only mode or "Authorization" header is absent
                                in JWT mode

   -s, --stripes                Number of stripes (default: 1)

   -c, --connections            Number of connections that need to be opened to Postgres
                                (default: 50)

       --timeout                Each connection's idle time before it is closed
                                (default: 180 sec)

   -i, --tx-iso                 Transaction isolation. read-commited / repeatable-read /
                                serializable
                                
       --enable-console         Enable API console. It is served at '/' and '/console'


Default environment variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can use environment variables to configure defaults instead of using flags:

.. note::
  When the equivalent flags for environment variables are used, the flags will take precedence.

For example:

.. code-block:: bash

   $ HASURA_GRAPHQL_DATABASE_URL=postgres://user:pass@host:5432/dbname graphql-engine serve


These are the environment variables which are available:

.. code-block:: none

   HASURA_GRAPHQL_DATABASE_URL          Postgres database URL
                                        <postgres/postgresql>://<user>:<password>@<host>:
                                        <port>/<db-name> Example:
                                        postgres://admin:mypass@mydomain.com:5432/mydb

   HASURA_GRAPHQL_PG_STRIPES            Number of stripes (default: 1)

   HASURA_GRAPHQL_PG_CONNECTIONS        Number of connections that need to be opened to
                                        Postgres (default: 50)

   HASURA_GRAPHQL_PG_TIMEOUT            Each connection's idle time before it is closed
                                        (default: 180 sec)

   HASURA_GRAPHQL_TX_ISOLATION          transaction isolation. read-committed /
                                        repeatable-read / serializable
                                        (default: read-commited)

   HASURA_GRAPHQL_SERVER_PORT           Port on which graphql-engine should be served

   HASURA_GRAPHQL_ACCESS_KEY            Secret access key, required to access this
                                        instance. If specified client needs to send
                                        'X-Hasura-Access-Key' header

   HASURA_GRAPHQL_AUTH_HOOK             The authentication webhook, required to
                                        authenticate incoming request

   HASURA_GRAPHQL_AUTH_HOOK_MODE        The authentication webhook mode, GET|POST
                                        (default: GET)

   HASURA_GRAPHQL_CORS_DOMAIN           The domain, including sheme and port,
                                        to allow CORS for

   HASURA_GRAPHQL_JWT_SECRET            The JSON containing type and the JWK used for
                                        verifying. e.g: `{"type": "HS256", "key":
                                        "<your-hmac-shared-secret>"}`,`{"type": "RS256",
                                        "key": "<your-PEM-RSA-public-key>"}
                                        Enable JWT mode, the value of which is a JSON

   HASURA_GRAPHQL_UNAUTHORIZED_ROLE     Unauthorized role, used when access-key is not sent
                                        in access-key only mode or "Authorization" header
                                        is absent in JWT mode

   HASURA_GRAPHQL_ENABLE_CONSOLE        Enable API console. It is served at
                                        '/' and '/console'
