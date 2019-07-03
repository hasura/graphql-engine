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
the ``v1/graphql`` API path. It should just be the hostname and any
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

Create a migration called ``init`` by exporting the current Postgres schema and
metadata from server:

.. code-block:: bash

   # (available after version v1.0.0-alpha45)
   # create migration files (note that this will only export public schema from postgres)
   hasura migrate create "init" --from-server

   # note down the version
   # mark the migration as applied on this server
   hasura migrate apply --version "<version>" --skip-execution


This command will create a new "migration" under the ``migrations`` directory
with the file name as ``<timestamp(version)>_init.up.yaml``. This file will
contain the required information to reproduce the current state of the server
including the Postgres (public) schema and Hasura metadata. The apply command
will mark this migration as "applied" on the server. If you'd like to read more
about the format of migration files, check out the :ref:`migration_file_format`.

.. note::

  If you need to export other schemas along with ``public``, you can name them using the
  ``--schema`` flag. 
  
  For example, to export schemas ``public``, ``schema1`` and ``schema2``,
  execute the following command:

  .. code-block:: bash

     hasura migrate create "init" --from-server --schema "public" --schema "schema1" --schema "schema2"

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
