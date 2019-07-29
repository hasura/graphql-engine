Clean-up event data
===================
.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


Hasura stores event data associated with Event Triggers in the metadata schema. If there are lots of events, the metadata tables can get huge and you may want to prune them. You can use any of the following options to prune your event data depending on your need.

Tables
------

Event Triggers have 2 tables managed by Hasura:

1. ``hdb_catalog.event_log``: This is the table that stores all captured events.
2. ``hdb_catalog.event_invocation_logs``: This is that table that stores all HTTP requests and responses.

Option 1: Clear only HTTP logs
------------------------------

.. code-block:: SQL

   DELETE FROM hdb_catalog.event_invocation_logs;

Option 2: Clear only processed events
-------------------------------------

.. code-block:: SQL

   DELETE FROM hdb_catalog.event_log
   WHERE delivered = true OR error = true;

Option 3: Clear all processed events and HTTP logs
--------------------------------------------------

This is the combination of Option 1 and Option 2.

.. code-block:: SQL

   DELETE FROM hdb_catalog.event_invocation_logs;

   DELETE FROM hdb_catalog.event_log
   WHERE delivered = true OR error = true;

Option 4: Clear all event data for a particular event trigger only
------------------------------------------------------------------

.. code-block:: SQL

   DELETE FROM
   hdb_catalog.event_invocation_logs
   WHERE event_id IN (
       SELECT id FROM hdb_catalog.event_log
       WHERE trigger_name = '<event_trigger_name>' );

   DELETE FROM
   hdb_catalog.event_log
   WHERE trigger_name = '<event_trigger_name>'
   AND (delivered = true OR error = true);


Option 5: Clear everything
--------------------------
.. admonition:: Warning
 
   This will clear all events including yet to be delivered events.

.. code-block:: SQL

   DELETE FROM hdb_catalog.event_invocation_logs;

   DELETE FROM hdb_catalog.event_log;
