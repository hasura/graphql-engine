.. meta::
   :description: Create an event trigger with Hasura
   :keywords: hasura, docs, event trigger, create

.. _create_trigger:

Creating an event trigger
=========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Event triggers can be created using the Hasura console or metadata APIs.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Via console

      Open the Hasura console, head to the ``Events`` tab and click on the ``Create`` button to open the
      page below:

      .. thumbnail:: /img/graphql/manual/event-triggers/create-event-trigger.png
         :alt: Create an event trigger

   .. tab:: Via API

      Make a request to the :ref:`create_event_trigger<create_event_trigger>` API.

Parameters
----------

**Trigger Name**

Unique name for event trigger.

**Schema/Table**

The postgres schema and table name on which the event trigger needs to be created.

**Trigger Operations**

The table operation on which the event trigger will be invoked.

**Webhook URL**

The HTTP(s) URL which will be called with the event payload on configured operation. Must be a ``POST`` handler. This URL
can be entered manually or can be picked up from an environment variable (*the environment variable needs to be set
before using it for this configuration*).

Advanced Settings
-----------------

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Via console

      .. thumbnail:: /img/graphql/manual/event-triggers/create-event-trigger-advanced-settings.png
         :alt: Advanced settings for event triggers

   .. tab:: Via API

      You can configure advanced settings via the :ref:`create_event_trigger<create_event_trigger>` API.

Listen columns for update
^^^^^^^^^^^^^^^^^^^^^^^^^

Update operations are special because you may want to trigger a webhook only if specific columns have changed in a row.
Choose the columns here which you want the update operation to listen to.

If a column is not selected here, then an update to that column will not trigger the webhook.


Retry Logic
^^^^^^^^^^^

Retry configuration is available in the "Advanced settings" when you create a trigger.

1. ``num_retries``: Number of times a failed invocation is retried. Default value is **0**.
2. ``interval_sec``: Number of seconds after which a failed invocation is retried. Default value is **10**.
3. ``timeout_sec``:: Number of seconds before which client closes the connection to the webhook. Default value is **60**.

Headers
^^^^^^^

Custom headers can be added to an event trigger. Each webhook request will have these headers added.

Each header has 3 parameters:

1. ``Key``: Name of the header e.g. Authorization or X-My-Header.
2. ``Type``: One of ``static`` or ``from env variable``. ``static`` means the value provided in the ``Value`` field is
   the raw value of the header. ``from env variable`` means the value provided in the ``Value`` field is the name of
   the environment variable in the GraphQL engine which will be resolved before sending the header.
3. ``Value``: The value of the header. Either a static value or the name of an environment variable.
