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

This page explains how to reset the state of migrations and create new migrations from the state that is on the server. 
This can be useful if the current migration state on your local machine is corrupted.

Step 1: Delete the local migration files
----------------------------------------

Move all your migrations to a backup folder, in case you may need them later.

You can use the following command:

.. code-block:: bash

   # move the contents of the migrations folder into the migrations_backup folder

   mv migrations migrations_backup

Step 2: Reset the migration history on the server
-------------------------------------------------

On the SQL tab of the Hasura console, run the following command:

.. code-block:: bash

   TRUNCATE hdb_catalog.schema_migrations;

Step 3: Pull the schema and metadata from the server
----------------------------------------------------

If the migrations were reset, then we will set up fresh migrations by pulling the schema and metadata from the server using the following commands:

.. code-block:: bash

   ## create migration files - "this will only export public schema from postgres"

   hasura migrate create "init" --from-server

.. code-block:: bash

   ## note down the version
   ## mark the migration as applied on this server
   
   hasura migrate apply --version "<version>" --skip-execution

.. code-block:: bash

   ## to also export the Hasura metadata and save it in the ``/metadata`` directory

   hasura metadata export   

.. note::

   If you are using schemas other than ``public``, use the ``--schema <schema_name>`` flag to indicate each one of them in the create command. This flag can be used multiple times.

Step 4: Verify the status of the migrations
-------------------------------------------

Run the following command to verify the migration status:

.. code-block:: bash

   hasura migrate status   

You should see the new migrations!  
