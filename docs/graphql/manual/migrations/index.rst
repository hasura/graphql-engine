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

How is Hasura state managed?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Hasura needs 2 pieces of information to expose the GraphQL API, the PG
database schema and the Hasura metadata which is used to describe the exposed
GraphQL API.

PG DB migrations
****************

The state of your database is managed via incremental SQL migration files.
These migration files can be applied one after the other to achieve the final
DB schema.

DB migration files can be generated incrementally and can by applied in parts to
reach particular checkpoints. They can be used to roll back the DB schemas as well.

Hasura metadata
***************

The state of Hasura metadata is managed via snapshots of the metadata. These s
napshots can be applied as a whole to configure Hasura to a state represented
in the snapshot.

Hasura metadata can be exported and imported as a whole.

Setting up migrations
---------------------

- :doc:`setup`


Advanced use cases
------------------

- :ref:`auto_apply_migrations`
- :ref:`manual_migrations`
- :ref:`roll_back_migrations`
- :doc:`advanced/seed-data-migration`
- :doc:`advanced/collaboration`

Reference documentation
-----------------------

- :ref:`migrations_how_it_works`
- :ref:`migration_file_format`
- :ref:`metadata_file_format`

.. toctree::
  :maxdepth: 1
  :hidden:

  Setting up <setup>
  Advanced use cases <advanced/index>
  Reference documentation <reference/index>
