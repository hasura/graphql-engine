.. meta::
   :description: Resetting Hasura migrations
   :keywords: hasura, docs, migration, reset migrations, clear migrations

.. _reset_migration:

Resetting Hasura migrations
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

If you have a lot of migrations and you need to squash them or your current migration state on the local machine is corrupted, you can reset the state and create new migrations from the state that is on the server.

Step 1: Delete the local migrations
-----------------------------------

In the local project directory, delete all its contents using the following command:

.. code-block:: bash

   rm -r migrations/*

Step 2: Reset the migration history on the server
-------------------------------------------------

On the SQL tab of the console, run the following command:

.. code-block:: bash

   TRUNCATE hdb_catalog.schema_migrations;

Step 3: Pull the schema and metadata from the server
----------------------------------------------------

We will set up fresh migrations by pulling the schema and metadata from the server using the following commands:

.. code-block:: bash

   ## create migration files - "this will only export public schema from postgres"

   hasura migrate create "init" --from-server

.. code-block:: bash

   ## note down the version
   ## mark the migration as applied on this server

   hasura migrate apply --version "<version>" --skip-execution

.. note::

   If you are using schemas other than ``public``, use ``--schema`` <schema_name> flag to indicate each one of them in the create command. This flag can be used multiple times.


Step 4: Verify the status of the migrations
-------------------------------------------

Run the following command to verify the migration status:

.. code-block:: bash

   hasura migrate status   

You should see the new migrations!   
