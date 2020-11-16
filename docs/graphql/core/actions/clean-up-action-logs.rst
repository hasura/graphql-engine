.. meta::
   :description: clean up action logs
   :keywords: hasura, docs, action, clean up, async actions

.. _clean_up_action_logs:

Clean up action logs
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------
Hasura stores action logs of asynchronous operations. As the table gets larger, you may want to prune them.

You can use any of the following options to prune your logs depending on your need.

The table involved
------------------

There is a specific table for action logs that is managed by Hasura:

- ``hdb_catalog.hdb_action_log``: This table stores all captured action logs.

Option 1: Get all logs
----------------------
.. code-block:: SQL
   
   SELECT * FROM hdb_catalog.hdb_action_log;

Option 2: Delete one log
------------------------
.. code-block:: SQL
   
   DELETE FROM hdb_catalog.hdb_action_log WHERE id = '<action-id>';

Option 3: Delete all logs
-------------------------
.. code-block:: SQL

   DELETE FROM hdb_catalog.hdb_action_log;

.. admonition:: Warning
 
   Deleting all logs is irreversible.   
