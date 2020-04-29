.. meta::
   :description: Manage Hasura migrations and metadata
   :keywords: hasura, docs, migration, metadata

.. _migrations_v1:

Migrations & Metadata (v1 config)
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

It is typical for developers to use some kind of "migration" tool to track
changes to the Postgres schema. Usually the SQL statements used to create the
tables, views etc. are stored as a single file or multiple files. Certain tools
also let you add an "up" and a "down" step so that you can roll back the
changes.

When you connect Hasura to a Postgres database and use the console to "track" a
table, a piece of information is added to the Hasura "metadata" (configuration)
indicating this table in Postgres should be exposed via GraphQL. Similarly,
most of the actions on the console update the Hasura metadata.

In the development phase, you'll be using the Hasura console to create and track
tables, create relationships, add permissions etc. When you need to move to a
new environment, it will become quite hard to re-do all these operations using
the console again on a fresh database. You might be looking for a way to export
the Hasura metadata so that you can apply it on another instance from a CI/CD
setup to reproduce the same setup. You might also be looking for a way to do a
dev --> staging --> production environment promotion scenario.

Well, you've come to the right place to address all these scenarios. Hasura is
bundled with a schema/metadata migration system which can be used to
declaratively track and version control all the changes happening to the
database.

.. note::

  This documentation is for **Hasura migrations config v1**. Hasura migrations
  config v2 was introduced in Hasura GraphQL engine version ``v1.2.0``

  See :ref:`migrations`

Simple use cases
----------------

We will split the use cases into two:

1. You're **already using a migration tool** for the Postgres schema migrations.

   In this case you only need to manage the Hasura metadata. See :ref:`manage_hasura_metadata_v1`
2. You're **not using any migration tool** for the Postgres schema.

   See :ref:`manage_migrations_v1`

Advanced use cases
------------------

- :ref:`auto_apply_migrations_v1`
- :ref:`manual_migrations_v1`
- :ref:`roll_back_migrations_v1`

Reference documentation
-----------------------

- :ref:`migration_file_format_v1`
- :ref:`metadata_file_format_v1`

.. toctree::
  :maxdepth: 1
  :hidden:

  Manage Postgres Schema and metadata <manage-migrations>
  Manage metadata <manage-metadata>
  Advanced use cases <advanced/index>
  Reference documentation <reference/index>
