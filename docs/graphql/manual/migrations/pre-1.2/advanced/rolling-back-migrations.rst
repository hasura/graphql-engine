.. meta::
   :description: Roll back Hasura migrations
   :keywords: hasura, docs, migration, roll back

.. _roll_back_migrations_old:

Rolling back applied migrations (pre v1.2)
==========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

If there are any issues with the migrations that are applied, you can
roll back the database and Hasura metadata to a desired version using the
``down`` migrations.

.. note::

   Rollbacks will only work if there are ``down`` migrations defined. The console
   will not generate ``down`` migrations for SQL statements executed from the
   ``SQL`` tab, even though you can add them as an ``up`` migration.

Rollback also means applying down migrations. Here are some example scenarios:

To roll back all the applied migrations, execute:

.. code-block:: bash

   hasura migrate apply --down all

To roll back the last 2 migration versions:

.. code-block:: bash

   hasura migrate apply --down 2

To roll back a particular migration version:

.. code-block:: bash

   hasura migrate apply --version 1550925483858 --type down

