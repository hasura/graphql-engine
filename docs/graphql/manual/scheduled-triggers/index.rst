.. meta::
   :description: Create a scheduled trigger with Hasura
   :keywords: hasura, docs, scheduled trigger

.. _scheduled_triggers:

Scheduled Triggers
==================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Scheduled triggers are used to execute custom business logic at a specific point in time. There are two types of timed events: cron-based or timestamp-based.

.. admonition:: Supported from
  
  Scheduled triggers are supported from versions ``v1.3.0-beta.1`` and above.

.. .. admonition:: Supported from
  
..   Scheduled triggers are supported from versions ``v.1.3.0`` and above.

**See:**

.. toctree::
   :maxdepth: 2
   :titlesonly:

   create-cron-trigger
   create-one-off-scheduled-event
