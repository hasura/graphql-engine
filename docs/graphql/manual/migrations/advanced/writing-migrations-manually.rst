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

.. note::

  For ``config v1``, see :ref:`manual_migrations_v1`.

Create migration manually
-------------------------

#. Set up the migration files:

   .. code-block:: bash

      hasura migrate create <name-of-migration>

   This command will create up and down migration SQL files in the
   ``migrations`` directory.

#. Edit the file and add your migration actions. For the file format and
   instructions on what actions can be added, refer to
   :ref:`migration file format <migration_file_format_v2>`.

#. The corresponding Hasura metadata changes, if any, can be made directly in
   the appropriate metadata file in the ``/metadata`` directory, refer to
   :ref:`metadata format <metadata_format_v2>`.

#. Apply the migration and metadata:

   .. code-block:: bash

      hasura migrate apply
      hasura metadata apply
