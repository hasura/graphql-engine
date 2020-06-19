.. meta::
   :description: Create a scheduled trigger with Hasura
   :keywords: hasura, docs, event trigger, scheduled trigger, create

.. _scheduled_triggers:

Scheduled triggers
==================

Scheduled triggers are used to execute custom business logic based on time. There are two types of timing events: cron-based or timestamp-based.

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Cron triggers
-------------

Cron triggers are used to reliably trigger HTTP endpoints to run custom business logic periodically based on a `cron schedule <https://en.wikipedia.org/wiki/Cron>`__. For example, you can create a cron trigger to generate an end-of-day sales report every weekday at 10pm.

To add a cron trigger, follow these steps:

Step 1. Navigate to Cron Triggers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Go to the ``Events`` tab in your Hasura console.
- Click ``Cron Triggers``.
- Click ``Create``.

.. thumbnail:: /img/graphql/manual/event-triggers/create-cron.png
   :alt: Adding a cron trigger

Step 2. Define the cron trigger
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the form opened by the above step, fill out these fields and click ``Create``:

- **Name**: Create a name for the cron trigger.
- **Webhook**: Enter the HTTP endpoint that should be triggered.
- **Cron schedule**: Enter a schedule for the cron. You can use the link provided to help `build a cron expression <https://crontab.guru/#*_*_*_*_*>`__, or use the ``Frequently used crons`` dropdown as a shortcut. Cron events are created based on the UTC timezone.
- **Payload**: The JSON payload which will be sent to the webhook.

.. thumbnail:: /img/graphql/manual/event-triggers/define-cron.png
   :alt: Defining a cron trigger
   :width: 660px

In this example, we're creating a cron trigger called ``eod_reports``, to trigger the webhook ``https://mywebhook.com/eod``. The cron schedule is set to ``0 22 * * 1-5``, which means "At 22:00 on every day-of-week from Monday through Friday" (you can check this `here <https://crontab.guru/#0_22_*_*_1-5>`__).

Step 3. Define advanced options (Optional)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you like, you can also define these values by expanding the ``Advanced`` section:

- **Headers**: List of headers to be sent to the webhook.
- **Retry configuration**: In case the call to the webhook fails.
- **Include in metadata**: If a cron trigger is included in the metadata, you can export it when the metadata of the graphql-engine is exported.
- **Comment**: Custom description of the cron trigger.

.. thumbnail:: /img/graphql/manual/event-triggers/advanced-cron.png
   :alt: Defining advanced options
   :width: 809px

Schedule & logs
^^^^^^^^^^^^^^^

Once you've created your cron trigger, you can see ``Pending events``, ``Processed events``, and ``Invocation logs`` in their respective tabs.

.. thumbnail:: /img/graphql/manual/event-triggers/pending-cron.png
   :alt: The tabs

One-off scheduled events
------------------------

One-off scheduled events are used to reliably trigger a HTTP webhook to run custom business logic at a particular timestamp. For example, you can create a scheduled event to send a reminder email two weeks after a user signs up.

You can schedule an event from your backend using the :ref:`metadata API <create_scheduled_event>`, or through the Hasura console.

To add a one-off scheduled event via the console, follow these steps:

Step 1. Navigate to One-off Scheduled Events
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Go to the ``Events`` tab in your Hasura console.
- Click ``One-off Scheduled Events``.
- Click ``Schedule an event``.

.. thumbnail:: /img/graphql/manual/event-triggers/one-off.png
   :alt: Adding a one-off scheduled event

Step 2. Define the scheduled event
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the form opened by the above step, fill out these fields and click ``Create scheduled event``:

- **Webhook**: Enter the HTTP endpoint that should be triggered.
- **Time**: Enter the time to trigger the event.
- **Payload**: The JSON payload which will be sent to the webhook.

.. thumbnail:: /img/graphql/manual/event-triggers/define-one-off.png
   :alt: Defining the scheduled event
   :width: 662px

Step 3. Define advanced options (Optional)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you like, you can also define these values by expanding the ``Advanced`` section:

- **Headers**: List of headers to be sent to the webhook.
- **Retry configuration**: In case the call to the webhook fails.
- **Comment**: Custom description of the scheduled trigger.

.. thumbnail:: /img/graphql/manual/event-triggers/advanced-one-off.png
   :alt: Defining advanced options
   :width: 809px

Schedule & logs
^^^^^^^^^^^^^^^

Once you've created your scheduled trigger, you can see ``Pending events``, ``Processed events``, and ``Invocation logs`` in their respective tabs.

.. thumbnail:: /img/graphql/manual/event-triggers/pending-one-off.png
   :alt: The tabs
