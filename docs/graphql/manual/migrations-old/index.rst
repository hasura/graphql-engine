.. meta::
   :description: Manage Hasura migrations and metadata
   :keywords: hasura, docs, migration, metadata

.. _migrations:

Migrations & Metadata
=====================

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

Simple use cases
----------------

We will split the use cases into two:

1. :doc:`You're already using a migration tool for the Postgres schema
   <manage-metadata>`. (like knex, TypeORM, Sequelize, Rails/Django
   migrations. In this case you only need to manage the Hasura metadata)
2. :doc:`You're not using any migration tool for the Postgres schema
   <manage-migrations>`. (Hasura will take care of the
   Postgres schema also)

Advanced use cases
------------------


- :doc:`auto-apply-migrations`
- :doc:`advanced/writing-migrations-manually`
- :doc:`advanced/rolling-back-migrations`

Reference documentation
-----------------------

- :doc:`reference/how-it-works`
- :doc:`reference/migration-file-format`
- :doc:`reference/metadata-file-format`

.. toctree::
  :maxdepth: 1
  :hidden:

  Manage Metadata <manage-metadata>
  Manage Migrations (Metadata + Postgres Schema) <manage-migrations>
  Advanced use cases <advanced/index>
  Reference documentation <reference/index>
