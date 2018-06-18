GraphQL Server Options
======================

For ``raven`` command these are the flags available

::

      --database-url       Postgres database URL
                           <scheme>://<user>:<password>@<host>:<port>/<db-name>
                           Example: http://admin:mypass@mydomain.com:5432/mydb

  Or either you can specifiy following options

      --host               Postgres server host
  -p, --port               Postgres server port
  -u, --user               Database user name
  -p, --password           Password of the user
  -d, --dbname             Database name to connect to
     
For ``serve`` subcommand these are the flags available

::

      --server-port        Port on which raven should be served (default: 8080)
      --access-key         Secret access key, required to access this instance.
                           If specified client needs to send 'X-Hasura-Access-Key'
                           header
      --cors-domain        The domain, including sheme and port, to allow CORS for
      --auth-hook          The authentication webhook, required to authenticate
                           incoming request
  -s, --stripes            Number of stripes
  -c, --connections        Number of connections that need to be opened to Postgres
      --timeout            Each connection's idle time before it is closed
  -i, --tx-iso             Transaction isolation. read-commited / repeatable-read /
                           serializable
      --root-dir           This static dir is served at / and takes precedence over
                           all routes
