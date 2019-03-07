.. _migrations:

Migrations
==========

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

When you're in the development phase, you'll be using the Hasura Console to
create and track tables, create relationships, add permissions etc. When you
need to move to a new environment, it will become quite hard to re-do all these
operations using the console again on a fresh database. You might be looking for
a way to declaratively export the Hasura metadata so that you can apply it on
another instance from a CI/CD setup to reproduce the same setup. You might be
looking for a way to do a dev --> staging --> production promotion scenario.

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

Advanced
--------

Here are some advanced use-cases are reference documentation:

- :doc:`auto-apply-migrations`
- :doc:`advanced/writing-migrations-manually`
- :doc:`advanced/migration-file-format`
- :doc:`advanced/metadata-file-format`

.. toctree::
  :maxdepth: 1
  :hidden:

  declarative-hasura-metadata
  declarative-postgres-schema-hasura-metadata
  Advanced use-cases and reference documentation <advanced/index>
