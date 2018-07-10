Server flags reference
======================

Every GraphQL engine command is structured as:

.. code-block::

   graphql-engine <server-flags> serve <command-flags>

Server flags
^^^^^^^^^^^^

For ``graphql-engine`` command these are the flags available

.. code-block:: none

  --database-url       Postgres database URL
                           <scheme>://<user>:<password>@<host>:<port>/<db-name>
                           Example: http://admin:mypass@mydomain.com:5432/mydb

  Or either you can specifiy following options

      --host               Postgres server host
  -p, --port               Postgres server port
  -u, --user               Database user name
  -p, --password           Password of the user
  -d, --dbname             Database name to connect to

Command flags
^^^^^^^^^^^^^

For ``serve`` subcommand these are the flags available

.. code-block:: none

   --server-port        Port on which graphql-engine should be served (default: 8080)
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


Default environment variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can use environment variables to configure defaults instead of using flags:

.. code-block::

   HASURA_GRAPHQL_DATABASE_URL=postgres://user:pass@host:5432/dbname graphql-engine serve


There are environment variables which are available:

For example,

.. code-block::

   graphql-engine <server-flags> serve <command-flags>


.. code-block::

   HASURA_GRAPHQL_DATABASE_URL   Postgres database URL
                                 <scheme>://<user>:<password>@<host>:<port>/<db-name>
                                 Example: http://admin:mypass@mydomain.com:5432/mydb

   HASURA_GRAPHQL_ACCESS_KEY     Secret access key, required to access this instance.
                                 If specified client needs to send 'X-Hasura-Access-Key'
                                 header

   HASURA_GRAPHQL_AUTH_HOOK      The authentication webhook, required to authenticate
                                 incoming request  

   HASURA_GRAPHQL_CORS_DOMAIN    The domain, including sheme and port, to allow CORS for


When the equivalent flags for environment variables are used, the flags will take precedence.
