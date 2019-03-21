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
also let's you add an "up" and a "down" step so that you can roll-back the
changes.

When you connect Hasura to a Postgres database and use the Console to "track" a
table, a piece of information is added to the Hasura "metadata" (configuration)
indicating this table in Postgres should be exposed via GraphQL. Similarly, when
most of the actions on the Console updates the metadata.

In the development phase, you'll be using the Hasura Console to create and track
tables, create relationships, add permissions etc. When you need to move to a
new environment, it will become quite hard to re-do all these operations using
the console again on a fresh database. You might be looking for a way to export
the Hasura metadata so that you can apply it on another instance from a CI/CD
setup to reproduce the same setup. You might also be looking for a way to do a
dev --> staging --> production environment promotion scenario.

Well, you've come to the right place. To address all these scenarios, Hasura is
bundled with a schema/metadata migration system which can be used to
declaratively track and version control all the changes happening to the
database. We will split the use cases into two: 

1. :doc:`You're already using a migration tool for the Postgres schema
   <declarative-hasura-metadata>`. (like knex, TypeORM, Sequelize, Rails/Django
   migrations)
2. :doc:`You're not using any migration tool for the Postgres schema
   <declarative-postgres-schema-hasura-metadata>`. (Hasura will take care of the
   Postgres schema also)

Advanced use cases
------------------


- :doc:`auto-apply-migrations`
- :doc:`advanced/writing-migrations-manually`
- :doc:`advanced/rolling-back-migrations`

Reference documentation
-----------------------

- :doc:`reference/how-does-it-work`
- :doc:`reference/migration-file-format`
- :doc:`reference/metadata-file-format`

.. toctree::
  :maxdepth: 1
  :hidden:

  Manage Metadata <declarative-hasura-metadata>
  Manage Migrations (Metadata + Postgres Schema) <declarative-postgres-schema-hasura-metadata>
  Advanced use-cases <advanced/index>
  Reference documentation <reference/index>
