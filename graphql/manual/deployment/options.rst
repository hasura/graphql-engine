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

Hasura ``graphql-engine`` accepts following environment variables in case if you don't want to specify corresponding flags.


+-----------------------------------+--------------------+
| Environment variable              | Flag               |
+===================================+====================+
| HASURA_GRAPHQL_DATABASE_URL       | ``--database-url`` |
+-----------------------------------+--------------------+
| HASURA_GRAPHQL_ACCESS_KEY         | ``--access-key``   |
+-----------------------------------+--------------------+
| HASURA_GRAPHQL_AUTH_HOOK          | ``--auth-hook``    |
+-----------------------------------+--------------------+
| HASURA_GRAPHQL_CORS_DOMAIN        | ``--cors-domain``  |
+-----------------------------------+--------------------+

**Note:** Always server flags take precedence over environment variables
