.. meta::
   :description: Roll back Hasura migrations
   :keywords: hasura, docs, migration, roll back

.. _roll_back_migrations:

Rolling back applied migrations
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

If there are any issues with changes made to the DB schema and Hasura metadata it
is possible to roll back their state to a previous stable version.

.. note::

  For ``config v1``, see :ref:`roll_back_migrations_v1`.

Rolling back database schema
----------------------------

Database schema rollbacks can be achieved via the ``down`` migrations generated
every time a schema change is made.

Here are some example scenarios:

To roll back a particular migration version:

.. code-block:: bash

   hasura migrate apply --version 1550925483858 --type down

To roll back the last 2 migration versions:

.. code-block:: bash

   hasura migrate apply --down 2

.. note::

   Rollbacks will only work if there are ``down`` migrations defined for a
   schema change.

   e.g. The console will not generate ``down`` migrations for SQL statements
   executed from the ``SQL`` tab.

Rolling back Hasura metadata
----------------------------

As Hasura metadata is managed via snapshots of the metadata, to roll back
Hasura metadata to a particular state you need the metadata snapshot at that
point. This is typically achieved by marking stable checkpoints of a project in
version control via commits.

.. code-block:: bash

   git checkout <stable-feature-commit>

   hasura metadata apply


