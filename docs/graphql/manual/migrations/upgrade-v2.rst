.. meta::
  :description: Upgrade to Hasura migrations v2
  :keywords: hasura, docs, migration, metadata


.. _migrations_upgrade_v2:

Upgrading to Hasura migrations config v2
========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

What has changed?
-----------------

In **config v1**, the PG schema migrations and Hasura metadata were both handled
using the same :ref:`migration files <migration_file_format_v1>` which were in
``yaml`` format. In **config v2**, these are managed separately in their own
directories in the Hasura project. Metadata is managed in its separate
:ref:`metadata directory <metadata_format_v2>` and PG schema migrations are
managed via :ref:`migration files <migration_file_format_v2>` that are now in
``SQL`` format.

Changes to exising workflows
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Due to the above mentioned changes, any workflows that involve applying migrations
have an additional step of applying metadata as well.

For example,

- any place where the ``hasura migrate apply`` command is used, it now needs
  to be followed by a ``hasura metadata apply`` command.

- if the ``cli-migrations`` Docker image is used for :ref:`auto applying migrations <auto_apply_migrations>`
  at server start, now the ``/metadata`` directory will also have to be mounted

Upgrade steps
-------------

Step 1: Upgrade to the latest CLI
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Config v2 is available since ``v1.2.0``.

Run:

.. code-block:: bash

  hasura update-cli

Step 2: Upgrade hasura project to v2
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In your project directory, run:

.. code-block:: bash

  hasura scripts update-project-v2

Your project directory and ``config.yaml`` should be updated to v2.