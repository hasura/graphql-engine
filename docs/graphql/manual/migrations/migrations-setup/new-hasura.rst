.. meta::
   :description: Migrations setup for a new Hasura instance
   :keywords: hasura, docs, migration, setup, new Hasura

.. _migrations_new_hasura:

Migrations for a new database and Hasura instance
=================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This guide will help you if you are about to start setting up your schema from scratch. You can use migrations to help track the database and GraphQL schema changes.
Refer to the :ref:`getting started guide <getting_started>` to set up a Hasura project.

Step 0: Disable console on the server
-------------------------------------

To use migrations effectively, the console on the server (which is served at ``/console``) should be disabled and all changes must go through the console served by the CLI. 
Otherwise, changes could be made through the server console and they will not be tracked by migrations.

So, the first step is to disable the console served by the GraphQL engine server. In order to do that, remove the ``--enable-console`` flag from the command that starts the server or set the following environment variable to false:

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
``https://my-graphql.herokuapp.com``. In case you've deployed this using Docker,
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

Step 3: Use the console from the CLI
------------------------------------

From this point onwards, instead of using the console at
``http://my-graphql.herokuapp.com/console`` you should use the console from the CLI
by running:

.. code-block:: bash

   # in project dir
   hasura console

Step 4: Add tables and see how a migration is added
---------------------------------------------------

As you use the Hasura console UI served by the CLI to make changes to your schema, migration files
are automatically generated in the ``migrations/`` directory in your project.

Let's add the following tables to our schema:

.. code-block:: sql

    author (id uuid, name text, rating integer)
    article (id uuid, title text, content text, author_id uuid)

In the ``migrations`` directory, we can find two directories called ``<timestamp>_create_table_public_author`` and ``<timestamp>_create_table_public_article`` containing an ``up.sql`` file and a ``down.sql`` file each.

If you'd like to read more about the format of migration files, check out the :ref:`migration_file_format_v2`.

.. note::

   Migrations are only created when using the console through the CLI.

Step 5: Apply the migrations and metadata on another instance of the GraphQL engine
-----------------------------------------------------------------------------------

Apply all migrations present in the ``migrations/`` directory on a new
instance at ``http://another-graphql-instance.herokuapp.com``:

.. code-block:: bash

   # in project dir
   hasura migrate apply --endpoint http://another-graphql-instance.herokuapp.com

In case you need an automated way of applying the migrations, take a look at the
:ref:`CLI-Migrations <auto_apply_migrations>` Docker image, which can start the
GraphQL engine after automatically applying the migrations which are
mounted into a directory.

Next, export the metadata from your current instance:

.. code-block:: bash

   # in project dir
   hasura metadata export --endpoint http://my-graphql.herokuapp.com

Now, import the metadata to your new instance:

.. code-block:: bash

   # in project dir
   hasura metadata apply --endpoint http://another-graphql-instance.herokuapp.com

This command tells Hasura to track tables, relationships etc.
If you'd like to read more about the format of metadata files, check out the :ref:`metadata_format_v2`.

If you now open the console of the new instance, you can see that the two tables have been created and are tracked:

.. thumbnail:: /img/graphql/manual/migrations/tracked-tables-new-hasura.png
   :alt: Schema for an article table
   :width: 40%

Step 6: Check the status of migrations
--------------------------------------

.. code-block:: bash

   # in project dir
   hasura migrate status

This command will print out each migration version present in the ``migrations``
directory and the ones applied on the database, along with a status text.

For example,

.. code-block:: bash

   $ hasura migrate status
   INFO <info>                                   version=<version>
   VERSION        NAME                           SOURCE STATUS  DATABASE STATUS
   1590493510167  create_table_public_author     Present        Present
   1590497881360  create_table_public_article    Present        Present

Such a migration status indicates that there are 2 migration versions in the
local directory and all of them are applied on the database.

If ``SOURCE STATUS`` indicates ``Not Present``, it means that the migration
version is present on the server, but not on the current user's local directory.
This typically happens if multiple people are collaborating on a project and one
of the collaborators forgot to pull the latest changes which included the latest
migration files, or another collaborator forgot to push the latest migration
files that were applied on the database. Syncing of the files would fix the
issue.

If ``DATABASE STATUS`` indicates ``Not Present``, it denotes that there are new
migration versions in the local directory which are not applied on the database
yet. Executing a ``migrate apply`` will resolve this.
