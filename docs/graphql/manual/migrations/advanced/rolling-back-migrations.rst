Rolling back applied migrations
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

If there is some issue with the migrations that are applied, you can
rollback the database and Hasura metadata to a desired version using the
``down`` migrations.

.. note::

   Rollbacks will only work if there are ``down`` migrations defined. Console
   will not generate ``down`` migrations for SQL statements executed from the
   ``SQL`` tab, even though you can add them as an ``up`` migration.

Rollback also means applying down migrations. Here are some example scenarios:

To rollback all the applied migrations, execute:

.. code-block:: bash

   hasura migrate apply --down

To rollback the last 2 migration versions:

.. code-block:: bash

   hasura migrate apply --down 2

To rollback a particular migration version:

.. code-block:: bash

   hasura migrate apply --version 1550925483858 --type down

