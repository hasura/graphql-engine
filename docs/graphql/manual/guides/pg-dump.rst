Backup and Restore Postgres data running on a Docker Container
===============================================================

Postgres comes with handy tools such as pg_dump and psql which lets us take backups and restore them with ease.

To backup data, we use the pg_dump tool:

.. code-block:: bash

    docker exec <postgres_container_name> pg_dump -U postgres <database_name> > backup.sql

.. note::
    The above command assumes you have postgres as the default user.

This command will create a text file named backup.sql containing all the data and schema of your database.

To import the data back into postgres, we use psql tool:

.. code-block:: bash

    cat backup.sql | docker exec -i <postgres_container_name> psql -U postgres -d <database_name> < backup.sql