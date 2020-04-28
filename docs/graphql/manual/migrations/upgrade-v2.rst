.. meta::
  :description: Upgrade to Hasura migrations v2
  :keywords: hasura, docs, migration, metadata


.. _migrations_upgrade_v2:

Upgrading to Hasura migrations v2
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Step 1: Upgrade to the latest CLI
---------------------------------

Run:

.. code-block:: bash

  hasura update-cli

Step 2: Upgrade hasura project to v2
------------------------------------

In your project directory, run:

.. code-block:: bash

  hasura scripts update-project-v2

Your project directory and ``config.yaml`` should be updated to v2.