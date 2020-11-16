.. meta::
   :description: clean up actions logs
   :keywords: hasura, docs, actions, clean up, async actions

.. _clean_up_actions_logs:

Clean up actions logs
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------
Hasura stores actions logs from asynchronous operations. If there are a lot of logs, the tables can get huge and you may want to prune them.
You can use any of the following options to prune your logs depending on your need.

The table involved
------------------

Actions has a table managed by Hasura:

- ``hdb_catalog.hdb_action_log``: This is the table that stores all captured logs.

Option 1: Get all logs
----------------------
.. code-block:: SQL
   
   SELECT * FROM hdb_catalog.hdb_action_log;

Option 2: Delete one log
------------------------
.. code-block:: SQL
   
   DELETE FROM hdb_catalog.hdb_action_log WHERE id = '<action-id>';

Option 3: Clear all logs
------------------------
.. admonition:: Warning
 
   Clearing all logs is irreversible.

.. code-block:: SQL

   DELETE FROM hdb_catalog.hdb_action_log;