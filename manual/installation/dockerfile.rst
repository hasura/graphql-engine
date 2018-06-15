Dockerfile Configuration
========================

.. code-block:: Dockerfile

   FROM hasuranightly/raven:94a0141

   CMD raven \
       --database-url $DATABASE_URL \
       serve \
       --server-port $PORT \
       --access-key $ACCESS_KEY


Options
-------

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


Server behaviour based on ``--access-key`` and ``--auth-hook``
--------------------------------------------------------------

Case 1: ``--access-key`` is set and ``--auth-hook`` is not set
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
``X-Hasura-Access-Key`` header is always expected. Server authorizes based on access key sent.
Required ``X-Hasura-*`` headers are expected in the request.


Case 2: ``--access-key`` is not set and ``--auth-hook`` is set
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
Server authorizes and fetches headers from provided hook. See :doc:`Authorization hook <../auth/hook>` .


Case 3: Both ``--access-key`` and ``--auth-hook`` are not set
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
No authorization required. Server expects required ``X-Hasura-*`` headers in the request


Case 4: Both ``--access-key`` and ``--auth-hook`` are set
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
If ``X-Hasura-Access-Key`` is sent then key is validated and hook is ignored. Server expects required ``X-Hasura-*`` headers in the request.
If ``X-Hasura-Access-Key`` header is not found then server authorizes and fetches headers from provided hook (See :doc:`Authorization hook <../auth/hook>`).
