.. meta::
   :description: Clean up async action logs
   :keywords: hasura, docs, action, clean up, async actions

.. _action_logs_clean_up:

Cleaning up async action logs
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Hasura stores action logs of :ref:`async actions <async_actions>` in a table in **the "hdb_catalog" schema of the Hasura metadata
database**.

As the table gets larger, you may want to prune it. You can use any of the following options to prune your logs depending on your need.

.. admonition:: Warning

   - Deleting logs is irreversible, so be careful with these actions.

   - Deleting logs while subscriptions for the response might still be open may result into the loss of data and ``null`` values been returned.  

The table involved
------------------

There is a specific table for action logs that is managed by Hasura:

- ``hdb_catalog.hdb_action_log``: This table stores all captured action logs.

Option 1: Delete log of a particular action invocation
------------------------------------------------------
.. code-block:: SQL

   DELETE FROM hdb_catalog.hdb_action_log WHERE id = '<async-action-id>';

Option 2: Delete all logs of a specific action
----------------------------------------------
.. code-block:: SQL

   DELETE FROM hdb_catalog.hdb_action_log WHERE action_name = '<action-name>';

Option 3: Delete all logs
-------------------------
.. code-block:: SQL

   DELETE FROM hdb_catalog.hdb_action_log;

Clearing data before a particular time period
---------------------------------------------

If you wish to keep recent data and only clear data before a particular time period
you can add the following time clause to your query's where clause:

.. code-block:: SQL

   -- units can be 'minutes', 'hours', 'days', 'months', 'years'
   created_at < now() - interval '<x> <units>'

For example: to delete all logs older than 3 months:

.. code-block:: SQL

   DELETE FROM hdb_catalog.hdb_action_log WHERE created_at < NOW() - INTERVAL '3 months';

See the `Postgres date/time functions <https://www.postgresql.org/docs/current/functions-datetime.html>`__
for more details.


.. admonition:: Additional Resources

  Introduction to Hasura Actions - `View Recording <https://hasura.io/events/webinar/hasura-actions/?pg=docs&plcmt=body&cta=view-recording&tech=>`__.