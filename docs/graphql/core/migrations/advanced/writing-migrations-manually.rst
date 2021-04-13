.. meta::
   :description: Write manual migrations for Hasura GraphQL engine
   :keywords: hasura, docs, migration, manual

.. _manual_migrations:

Writing migrations manually
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

While the Hasura console can auto generate migrations for every action,
sometimes you might want to write the migrations yourself, by hand. Using the
Hasura CLI, you can bootstrap these migration files and write the SQL for the
Postgres schema.

Create migration manually
-------------------------

#. Set up the migration files:

   .. code-block:: bash

      hasura migrate create <name-of-migration> --database-name <database-name>

   This command will create up and down migration SQL files in the
   ``migrations`` directory.

#. Edit the file and add your migration actions. For the file format and
   instructions on what actions can be added, refer to
   :ref:`migration file format <migration_file_format_v2>`.

#. The corresponding Hasura metadata changes, if any, can be made directly in
   the appropriate metadata file in the ``/metadata`` directory, refer to
   :ref:`metadata format <metadata_format_v2>`.

#. Apply the metadata and migrations:

   .. code-block:: bash

      hasura metadata apply
      hasura migrate apply --database-name <database-name>
      hasura metadata reload

