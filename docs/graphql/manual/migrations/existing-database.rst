Migrations for an existing database and Hasura instance
=======================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide is to be followed if you already have set up a database and Hasura
instance and now want to start using migrations to help you track the database
and GraphQL schema changes.

Step 0: Disable console on Server
---------------------------------

To use migrations effectively, console on the Server (which is served at
``/console``) should be disabled and all changes must go through the console
served by CLI. Otherwise, changes could be made through the server-console and
they will not be tracked by migrations.

So, the first step is to disable console served by the GraphQL Engine server. In
order to do that remove ``--enable-console`` flag from the command that starts
the server or set the following environment variable to false:

.. code-block:: bash

   HASURA_GRAPHQL_ENABLE_CONSOLE=false

.. note::

   If this is set in YAML, make sure you quote the word false, i.e.
   ``HASURA_GRAPHQL_ENABLE_CONSOLE: "false"``.

Step 1: Install the Hasura CLI
------------------------------

Follow the instructions in :ref:`install_hasura_cli`.

Step 2: Set up a project directory
----------------------------------

Execute the following command. For the endpoint referred here, let's say you've
deployed the GraphQL engine on Heroku, then this endpoint is:
``https://my-graphql.herokuapp.com``. In case you've deployed this using Docker,
the URL might be ``http://xx.xx.xx.xx:8080``. This endpoint should not contain
the ``v1alpha1/graphql`` API path. It should just be the hostname and any
sub-path if it is configured that way. 

.. code-block:: bash

  hasura init --directory my-project --endpoint http://my-graphql.herokuapp.com

  cd my-project

This will create a new directory called ``my-project`` with a ``config.yaml``
file and ``migrations`` directory. This directory structure is mandatory to use
Hasura migrations. You can commit this directory to version control.

.. note::

   In case there is an admin secret set, you can set it as an environment
   variable ``HASURA_GRAPHQL_ADMIN_SECRET=<your-admin-secret`` on the local
   machine and the the CLI will use it. You can also use it as a flag to CLI:
   ``--admin-secret "<your-admin-secret>"``.

Step 3: Initialize the migrations as per your current state
-----------------------------------------------------------

- Use ``pg_dump`` to export the database schema:

  If Postgres is running in docker, we can use the ``pg_dump``
  command bundled within the ``postgres`` docker container. If you have
  ``pg_dump`` installed on your machine, you could use that as well.

  .. code-block:: bash

     # get the container id for postgres
     docker ps

     # dump the public schema into public-schema.sql (repeat for other schemas)
     docker exec <postgres-container-id> pg_dump -O -x -U postgres --schema-only --schema public > public-schema.sql

  If Postgres is on Heroku or elsewhere, install ``pg_dump`` on your machine and
  use it. It comes with a standard Postgres installation which you can download
  and install from `here <https://www.postgresql.org/download/>`__.

  .. code-block:: bash

     # Get the DATABASE_URL from Heroku Dashbaord -> Settings -> Reveal Config Vars
     # dump the public schema into public-schema.sql (repeat for other schemas)
     pg_dump -O -x "<DATABASE_URL>" --schema-only --schema public > public-schema.sql

  This command will create ``public-schema.sql`` which contains the SQL
  definitions for the public schema.

- Clean up the SQL file to remove some un-necessary statements:

  .. code-block:: bash

     # POST the SQL to a serverless function and save the response
     curl --data-binary @public-schema.sql https://hasura-edit-pg-dump.now.sh > public-schema-edited.sql

  (The source code for this function can be found on `GitHub <https://github.com/hasura/graphql-engine/tree/master/scripts/edit-pg-dump>`__ along with a bash script if you'd prefer that.)

- Create a migration called ``init`` using this SQL file and the metadata that
  is on the server right now:

  .. code-block:: bash

     # create migration files
     hasura migrate create "init" --sql-from-file "public-schema-edited.sql" --metadata-from-server

     # note down the version
     # mark the migration as applied on this server
     hasura migrate apply --version "<version>" --skip-execution

  This command will create a new "migration" under the ``migrations`` directory
  with the file name as ``<timestamp(version)>_init.up.yaml``. This file will
  contain the required information to reproduce the current state of the server
  including the Postgres schema and Hasura metadata. The apply command will mark
  this migration as "applied" on the server. If you'd like to read more about
  the format of migration files, check out the :ref:`migration_file_format`.

.. note::

  Migration version cannot be "0". i.e. the files cannot be of the form ``0_<something>.up.yaml``

Step 4: Use the console from the CLI
------------------------------------

From this point onwards, instead of using the console at
``http://my-graphql.herokuapp.com/console`` you should use the console from CLI
by running:

.. code-block:: bash

   # in project dir
   hasura console

Step 5: Add a new table and see how a migration is added
--------------------------------------------------------

As you use the Hasura console UI to make changes to your schema, migration files are automatically generated
in the ``migrations/`` directory in your project.

.. note::

   Migrations are only created when using the console through CLI.

Step 6: Apply the migrations on another instance of GraphQL engine
------------------------------------------------------------------

Apply all migrations present in the ``migrations/`` directory on a new
instance at ``http://another-graphql-instance.herokuapp.com``:

.. code-block:: bash

   # in project dir
   hasura migrate apply --endpoint http://another-graphql-instance.herokuapp.com

In case you need an automated way of applying the migrations, take a look at the
:doc:`CLI-Migrations <auto-apply-migrations>` docker image, which can start
GraphQL Engine after automatically applying the migrations which are
mounted into a directory.  

Step 7: Check status of migrations
----------------------------------

.. code-block:: bash

   # in project dir
   hasura migrate status

This command will print out each migration version present in the ``migrations``
directory and the ones applied on the database, along with a status text.

For example,

.. code-block:: bash

   $ hasura migrate status
   VERSION        SOURCE STATUS  DATABASE STATUS
   1550925483858  Present        Present
   1550931962927  Present        Present
   1550931970826  Present        Present

Such a migration status indicate that there are 3 migration versions in the
local directory and all of them are applied on the database.

If ``SOURCE STATUS`` indicates ``Not Present``, it means that the migration
version is present on the server, but not on the current user's local directory.
This typically happens if multiple people are collaborating on a project and one
of the collaborator forgot to pull the latest changes which included the latest
migration files or another collaborator forgot to push the latest migration
files that were applied on the database. Syncing of the files would fix the
issue.

If ``DATABASE STATUS`` indicates ``Not Present``, it denotes that there are new
migration versions in the local directory which are not applied on the database
yet. Executing a ``migrate apply`` would take care of such scenarios.
