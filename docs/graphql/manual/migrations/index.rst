.. meta::
   :description: Manage Hasura migrations and metadata
   :keywords: hasura, docs, migration, metadata

.. _migrations:

Migrations & Metadata
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

It is a typical requirement to export an existing Hasura "setup" so that you can
apply it on another instance to reproduce the same setup. For example, to achieve
a dev -> staging -> production environment promotion scenario.

.. note::

  This documentation is for **Hasura migrations v2** which was introduced in
  Hasura GraphQL engine version ``v1.2``. (See :ref:`upgrade guide <migrations_upgrade_v2>`).

  For earlier versions, see :ref:`migrations_v1`.

How is Hasura state managed?
----------------------------

Hasura needs 2 pieces of information to recreate your GraphQL API, the underlying
PG database schema and the Hasura metadata which is used to describe the exposed
GraphQL API.

The :ref:`Hasura CLI <hasuracli-manual>` lets you manage these pieces of
information as you build your project via:

Database migration files
^^^^^^^^^^^^^^^^^^^^^^^^

The state of your PG database is managed via incremental SQL migration files.
These migration files can be applied one after the other to achieve the final
DB schema.

DB migration files can be generated incrementally and can by applied in parts to
reach particular checkpoints. They can be used to roll-back the DB schema as well.

Hasura metadata files
^^^^^^^^^^^^^^^^^^^^^

The state of Hasura metadata is managed via snapshots of the metadata. These
snapshots can be applied as a whole to configure Hasura to a state represented
in the snapshot.

Hasura metadata can be exported and imported as a whole.

Setting up migrations
---------------------

Follow the :ref:`setup_migrations` guide.


Advanced use cases
------------------

- :ref:`auto_apply_migrations`
- :ref:`manual_migrations`
- :ref:`roll_back_migrations`
- :ref:`seed_data_migration`
- :ref:`collaborate_migrations`

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
  Upgrading to migrations v2 <upgrade-v2>
  Pre v1.2 <pre-1.2/index>
