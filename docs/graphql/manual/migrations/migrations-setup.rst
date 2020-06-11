.. meta::
   :description: Migrations setup for an existing Hasura instance
   :keywords: hasura, docs, migration, setup, existing Hasura

.. _migrations_setup:

Setting up Hasura migrations
============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

If you don’t already use any tool to manage your Postgres schema, you can use Hasura to do that for you. 
Hasura has a CLI which will help you save each action that you do on the console, including creating
tables/views and schema modifying SQL statements, as SQL files.
These files are called migrations and they can be applied and rolled back step-by-step. These files
can be version controlled and can be used with your CI/CD system to make incremental updates.

Let's say we have the following two tables in our schema:

.. code-block:: sql

    author (id uuid, name text, rating integer)
    article (id uuid, title text, content text, author_id uuid)

Now we want to set up migrations for starting with this schema.

Step 0: Disable console on the server
-------------------------------------

To use migrations effectively, the console on the server (which is served at ``/console``) should be
disabled and all changes must go through the console served by the CLI. Otherwise, changes could be
made through the server console and they will not be tracked by migrations.

So, the first step is to disable the console served by the GraphQL engine server. In order to do
that, remove the ``--enable-console`` flag from the command that starts the server or set the
following environment variable to ``false``:

.. code-block:: bash

    HASURA_GRAPHQL_ENABLE_CONSOLE=false

.. note::

   If this is set in YAML, make sure you quote the word false, i.e.
   ``HASURA_GRAPHQL_ENABLE_CONSOLE: "false"``.

Step 1: Install the Hasura CLI
------------------------------

Follow the instructions in :ref:`Installing the Hasura CLI <install_hasura_cli>`.

Step 2: Set up a project directory
----------------------------------

For the endpoint referred here, let's say you've
deployed the GraphQL engine on Heroku, then this endpoint is:
``https://my-graphql.herokuapp.com``. In case you've deployed Hasura using Docker,
the URL might be ``http://xx.xx.xx.xx:8080``. In any case, the endpoint should **not** contain
the ``v1/graphql`` API path. It should just be the hostname and any
sub-path if it is configured that way. 

Let's set up a project directory by executing the following command:

.. code-block:: bash

  hasura init --directory my-project --endpoint http://my-graphql.herokuapp.com

  cd my-project

This will create a new directory called ``my-project`` with a ``config.yaml``
file, a ``migrations`` directory and a ``metadata`` directory. This directory structure is mandatory to use
Hasura migrations. 

These directories can be committed to version control.

.. note::

   In case there is an admin secret set, you can set it as an environment
   variable ``HASURA_GRAPHQL_ADMIN_SECRET=<your-admin-secret>`` on your local
   machine and the CLI will use it. You can also use it as a flag to CLI commands:
   ``--admin-secret '<your-admin-secret>'``.

.. _migrations_setup_initialize:

Step 3: Initialize the migrations and metadata as per your current state
------------------------------------------------------------------------

If you have already set up your database and GraphQL API, you need to initialize your
database migrations and Hasura metadata with the current state of the database.

Step 3.1: Initialize database migrations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create a migration called ``init`` by exporting the current Postgres schema from the server:

.. code-block:: bash

   # create migration files (note that this will only export the public schema from postgres)
   hasura migrate create "init" --from-server

   # note down the version

   # mark the migration as applied on this server
   hasura migrate apply --version "<version>" --skip-execution

This command will create a new directory named ``<timestamp>_init`` inside the ``migrations`` directory. 
In the newly created directory, there's a file named ``up.sql``.
This file will contain the required information to reproduce the current state of the server
including the Postgres (public) schema. If you'd like to read more about the format of migration files,
check out the :ref:`migration_file_format_v2`.

The apply command will mark this migration as "applied" on the server.

.. note::

  If you need to export other schemas along with ``public``, you can name them using the
  ``--schema`` flag. 
  
  For example, to export schemas ``public``, ``schema1`` and ``schema2``,
  execute the following command:

  .. code-block:: bash

     hasura migrate create "init" --from-server --schema "public" --schema "schema1" --schema "schema2"

Step 3.2: Initialize Hasura metadata
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Export the Hasura metadata from the server:

.. code-block:: bash

   # export the metadata
   hasura metadata export

This command will export the current Hasura metadata as a bunch of YAML files in the ``metadata`` directory.

If you'd like to read more about the format of metadata files, check out the :ref:`metadata_format_v2`.

Step 3.3: Set up version control for your project directory
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Set up version control and commit the project status.

.. code-block:: bash

  # in project dir

  # initialize version control
  git init

  # commit initial project status
  git add .
  git commit -m "init"


Step 4: Use the console from the CLI
------------------------------------

From this point onwards, instead of using the console at
``http://my-graphql.herokuapp.com/console`` you should use the console from the CLI
by running:

.. code-block:: bash

   # in project dir
   hasura console

Step 5: Add a new table and see how migrations and metadata is updated
----------------------------------------------------------------------

As you use the Hasura console UI served by the CLI to make changes to your schema, database migration
files are automatically generated in the ``migrations/`` directory and the metadata is
exported in the ``metadata/`` directory of your project.

Let's create the following table ``address (id uuid, street text, zip text, city text, country text, author_id int)``
and then create a foreign-key to the ``author`` table via the ``author_id -> id`` columns.

In the ``migrations`` directory, we can find new directories called ``<timestamp>_create_table_public_address``
and ``<timestamp>_set_fk_public_address_author_id`` containing an ``up.sql`` and a ``down.sql`` migration files
for the changes we made.

You can also go ahead and add permissions and create relationships for the address table.
The related metadata changes will automatically be exported into the ``metadata`` directory.

.. note::

   Migrations are only created when using the console through the CLI.

Step 6: Squash migrations and add checkpoints
---------------------------------------------

As you keep using the console via the CLI to make changes to the schema, new
migration files will keep getting generated and the metadata files will keep getting
updated automatically.

Typically while adding a feature a lot of incremental migration files get
created for each of the small tasks that you did to achieve the feature. To
improve maintainability of the migration files and to ensure you can go back to a particular
version of the metadata, it is recommended that you squash your migration files and
commit the project status in version control whenever you reach a logical checkpoint in your feature
development.

The following command will squash all migration files from the given migration to the latest
migration into a single migration file.

.. code-block:: bash

  hasura migrate squash --name "<feature-name>" --from <start-migration-version>

  # note down the version

  # mark the squashed migration as applied on this server
  hasura migrate apply --version "<squash-migration-version>" --skip-execution

Commit the project status into version control.

.. code-block:: bash

  # initialize version control if not done already
  git init

  # commit project status
  git add .
  git commit -m "<feature-name>"

.. note::

   The version control set up should typically be done right after :ref:`Step 3 <migrations_setup_initialize>`


Step 7: Apply the migrations and metadata on another instance of the GraphQL engine
-----------------------------------------------------------------------------------

Apply all migrations present in the ``migrations/`` directory and the metadata present
in the ``metadata/`` directory on a new instance at ``http://another-graphql-instance.herokuapp.com``:

.. code-block:: bash

   # in project dir
   hasura migrate apply --endpoint http://another-graphql-instance.herokuapp.com
   hasura metadata apply --endpoint http://another-graphql-instance.herokuapp.com

In case you need an automated way of applying the migrations and metadata, take a look at the
:ref:`cli-migrations <auto_apply_migrations>` Docker image, which can start the
GraphQL engine after automatically applying the migrations and metadata which are
mounted onto directories.

If you now open the console of the new instance, you can see that the three tables have
been created and are tracked:

.. thumbnail:: /img/graphql/manual/migrations/tracked-tables.png
   :alt: Tracked tables from Hasura migrations
   :width: 40%

Step 8: Check the status of migrations
--------------------------------------

.. code-block:: bash

   # in project dir
   hasura migrate status

This command will print out each migration version present in the ``migrations``
directory along with its name, source status and database status.

For example,

.. code-block:: bash

   $ hasura migrate status
   VERSION        NAME                           SOURCE STATUS  DATABASE STATUS
   1590493510167  init                           Present        Present
   1590497881360  create_table_public_address    Present        Present

Such a migration status indicates that there are 2 migration versions in the
local directory and both of them are applied on the database.

If ``SOURCE STATUS`` indicates ``Not Present``, it means that the migration
version is present on the server, but not on the current user's local directory.
This typically happens if multiple people are collaborating on a project and one
of the collaborators forgot to pull the latest changes which included the latest
migration files, or another collaborator forgot to push the latest migration
files that were applied on the database. Syncing of the files would fix the
issue.

If ``DATABASE STATUS`` indicates ``Not Present``, it denotes that there are new
migration versions in the local directory which are not applied on the database
yet. Executing ``hasura migrate apply`` will resolve this.
