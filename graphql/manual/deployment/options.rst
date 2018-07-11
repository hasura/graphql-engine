Hasura GraphQL Engine Options
=============================

For ``graphql-engine`` command these are the flags available

::

      --database-url       Postgres database URL
                           <postgres/postgresql>://<user>:<password>@<host>:<port>/<db-name>
                           Example: postgres://admin:mypass@mydomain.com:5432/mydb

  Or either you can specifiy following options

      --host               Postgres server host
  -p, --port               Postgres server port
  -u, --user               Database user name
  -p, --password           Password of the user
  -d, --dbname             Database name to connect to

For ``serve`` subcommand these are the flags available

::

      --server-port        Port on which graphql-engine should be served (default: 8080)
      --access-key         Secret access key, required to access this instance.
                           If specified client needs to send 'X-Hasura-Access-Key'
                           header
      --cors-domain        The domain, including sheme and port, to allow CORS for
      --disable-cors       Disable CORS handling
      --auth-hook          The authentication webhook, required to authenticate
                           incoming request
  -s, --stripes            Number of stripes
  -c, --connections        Number of connections that need to be opened to Postgres
      --timeout            Each connection's idle time before it is closed
  -i, --tx-iso             Transaction isolation. read-commited / repeatable-read /
                           serializable
      --root-dir           This static dir is served at / and takes precedence over
                           all routes
      --enable-console     Enable API console. It is served at '/' and '/console'


Graphql Engine Environment Variables
------------------------------------

There are environment variables which are available:


::

      HASURA_GRAPHQL_DATABASE_URL   Postgres database URL
                                    <scheme>://<user>:<password>@<host>:<port>/<db-name>
                                    Example: http://admin:mypass@mydomain.com:5432/mydb

      HASURA_GRAPHQL_ACCESS_KEY     Secret access key, required to access this instance.
                                    If specified client needs to send 'X-Hasura-Access-Key'
                                    header

      HASURA_GRAPHQL_AUTH_HOOK      The authentication webhook, required to authenticate
                                    incoming request  

      HASURA_GRAPHQL_CORS_DOMAIN    The domain, including sheme and port, to allow CORS for


**Note:** When the equivalent flags for environment variables are used, the flags will take precedence.
