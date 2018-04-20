=====================
Exporting data as SQL
=====================

Since Hasura uses Postgres, you can export your schema and data with `pg_dump <https://www.postgresql.org/docs/9.3/static/app-pgdump.html>`_.

You will need to have ``pg_dump`` installed.

#. Forward the ``postgres`` microservice to your localhost by running the following command from your directory:

   .. code-block:: bash

      $ hasura microservice port-forward postgres -n hasura --local-port 6432

   After running the above command, the ``postgres`` microservice will be running on your localhost on the port 6432.

#. Then simply run the ``pg_dump`` command to get your data as SQL.

   .. code-block:: bash

      $ pg_dump -U admin hasuradb --host=localhost --port=6432 > backup.sql

   After the above command succeeds, you will have your backup in ``backup.sql``. You can replace ``backup.sql`` with path to the file you wish to have your data in.  
