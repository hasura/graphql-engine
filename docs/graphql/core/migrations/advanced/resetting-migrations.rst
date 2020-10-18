.. meta::
   :description: Resetting Hasura migrations
   :keywords: hasura, docs, migration, reset migrations, clear migrations

.. _reset_migration:

Reset Hasura migrations
==============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Step 1: Delete the local migrations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the local migrations directory, delete all its contents using the following command:

.. code-block:: bash

   rm migrations/*

Step 2: Reset the migration history on the server
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On the SQL tab of the console, run the following command:

.. code-block:: bash

   TRUNCATE hdb_catalog.schema_migrations;

Step 3: Pull the schema and metadata from server
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We will set up fresh migrations by pulling the schema and metadata from the server using the following commands:

.. code-block:: bash

   hasura migrate create "init" --from-server

   hasura migrate apply --version "<version>" --skip-execution


Step 4: Verify the status of the migrations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Run the following command to verify status of migration:

.. code-block:: bash

   hasura migrate status   