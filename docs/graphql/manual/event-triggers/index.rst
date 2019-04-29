Event Triggers
==============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Hasura can be used to create event triggers on tables in the Postgres database. Event triggers reliably capture
events on specified tables and invoke webhooks to carry out any custom logic.

.. thumbnail:: ../../../img/graphql/manual/event-triggers/database-event-triggers.png
   :class: no-shadow

Events can be of the following types:

- INSERT: When a row is inserted into a table
- UPDATE: When a row is updated in a table
- DELETE: When a row is deleted from a table
- MANUAL: Using the console, an event can be triggered on a row. See :doc:`Manual Triggers <./manual-trigger>`

**See:**

.. toctree::
   :maxdepth: 2
   :titlesonly:

   create-trigger
   manual-trigger
   payload
   serverless
   samples
