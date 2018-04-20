.. .. meta::
   :description: Manual for accessing postgres directly
   :keywords: hasura, docs, postgres, sql, import sql, data, import data

Importing an existing PostgreSQL database
=========================================

This section is about moving your existing Postgres database to Hasura.

You will need:

- :doc:`hasura CLI <../install-hasura-cli>`
- `pg_dump <https://www.postgresql.org/docs/9.3/static/app-pgdump.html>`_
- `pg_restore <https://www.postgresql.org/docs/9.2/static/app-pgrestore.html>`_
- A Hasura project. Run the following command to get one:

  .. code-block:: bash

     $ hasura quickstart base

Moving the Schema
-----------------

#. Generate an empty :doc:`Hasura migration <../data/data-migration>`. Run the following command from your project directory.

   .. code-block:: bash

      $ hasura migration generate data_import

   This command will generate four migration files in the ``migrations`` directory of your Hasura project. They will look like:

   - 1524223609768_data_import.down.sql
   - 1524223609768_data_import.down.yaml
   - 1524223609768_data_import.up.sql
   - 1524223609768_data_import.up.yaml

#. Take a ``schema-only`` dump of your existing database using `pg_dump <https://www.postgresql.org/docs/9.3/static/app-pgdump.html>`_ add add it to the migration

   .. admonition:: Important

      You have to use the ``-s`` tag to take the ``schema-only`` pg_dump.

   Your ``pg_dump`` command will look something like:

   .. code-block:: bash

      $ pg_dump -s dbname --host=localhost --port=5432 > schema.dump

#. Copy this schema dump content in the migration that has ``.up.sql`` in the end (in the above example, ``1524223609768_data_import.up.sql``).

   You can run:

   .. code-block:: bash

      $ cat schema.dump > migrations/1524223609768_data_import.up.sql

#. Apply your schema:

   .. code-block:: bash

     $ hasura migration apply

#. Track these tables so that they are accessible by the ``Hasura APIs``. Go to the API Console:

   .. code-block:: bash

      $ hasura api-console

#. Go to the ``Data`` section. You can find your tables under the ``Untracked Tables``. Just click on ``Add all`` to track them.

   .. image:: ../../img/manual/data/track-untracked-tables.png

Your schema has been migrated. Lets move on to migrating the data.

Moving the data
---------------

#. Take a ``data-only`` dump of your existing database in ``custom format`` using pg_dump.

   .. admonition:: Important

      The flags for ``data-only`` and ``custom format`` are ``--data-only`` and ``-Fc`` respectively.

   Your ``pg_dump`` command will look something like:

   .. code-block:: bash

      $ pg_dump --data-only -Fc dbname --host=localhost --port=5432 > data.dump

#. Forward your Hasura ``postgres`` microservice to your localhost. Run the following command from your project directory to forward it to the 6432 port of your localhost.

   .. code-block:: bash

      $ hasura microservice port-forward postgres -n hasura --local-port 6432

#. Use `pg_restore <https://www.postgresql.org/docs/9.2/static/app-pgrestore.html>`_ on the ``postgres`` microservice running at ``localhost:6432`` to restore this data. Make sure to use ``--disable-triggers`` flag so that the schema constraints do not interfere with the restoration.

   .. code-block:: bash

      $ pg_restore --disable-triggers -U admin -d hasuradb --host=localhost --port=6432

That's it. You can immediately start using the GraphQL or JSON APIs over this newly imported data.

Verifying the migration
-----------------------

#. Check if the schema and data has been migrated. Go to the API-Console and go to the ``Data`` section on top.

   .. code-block:: bash

      $ hasura api-console

#. Go to the API-Explorer section and try making queries.
