Migrations for a new database and Hasura instance
=================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide will help you if you are about to start setting up your schema from
scratch. You can use migrations to help track the database and GraphQL schema
changes.


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

Step 3: Open console from CLI
-----------------------------

Instead of using the console at ``http://my-graphql.herokuapp.com/console`` you
should now use the console by running: 

.. code-block:: bash

   # in project dir
   hasura console

Step 4: Add a new table and see how a migration is added
--------------------------------------------------------

As you use the Hasura console UI to make changes to your schema, migration files
are automatically generated in the ``migrations/`` directory in your project. 

Step 5: Apply the migrations on another instance of GraphQL engine
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

Step 6: Check status of migrations
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
