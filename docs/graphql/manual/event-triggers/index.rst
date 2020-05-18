.. meta::
   :description: Manage event triggers with Hasura
   :keywords: hasura, docs, event trigger

.. _event_triggers:

Event Triggers
==============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Hasura can be used to create event triggers on tables in the Postgres database. Event triggers reliably capture
events on specified tables and invoke webhooks to carry out any custom logic.

.. thumbnail:: /img/graphql/manual/event-triggers/database-event-triggers.png
   :class: no-shadow
   :width: 80%
   :alt: Hasura event trigger architecture

Events can be of the following types:

- INSERT: When a row is inserted into a table
- UPDATE: When a row is updated in a table
- DELETE: When a row is deleted from a table
- MANUAL: Using the console or API, an event can be triggered manually on a row

.. note::

  Event webhook notifications will be delivered at least once, and may arrive out of order with 
  respect to the underlying event.

**See:**

.. toctree::
   :maxdepth: 2
   :titlesonly:

   create-trigger
   payload
   serverless
   samples
   Invoke trigger via console <invoke-trigger-console>
   clean-up
