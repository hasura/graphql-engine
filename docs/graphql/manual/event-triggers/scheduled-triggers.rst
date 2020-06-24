.. meta::
   :description: Create a scheduled trigger with Hasura
   :keywords: hasura, docs, cron trigger, scheduled trigger, create

.. _scheduled_triggers:

Scheduled triggers
==================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Scheduled triggers are used to execute custom business logic at a specific point in time. There are two types of timed events: cron-based or timestamp-based.

.. admonition:: Supported from
  
  Scheduled triggers are supported from versions ``v1.3.0-beta.1`` and above.

.. .. admonition:: Supported from
  
..   Scheduled triggers are supported from versions ``v.1.3.0`` and above.

Cron triggers
-------------

Cron triggers are used to reliably trigger HTTP endpoints to run custom business logic periodically based on a `cron schedule <https://en.wikipedia.org/wiki/Cron>`__. For example, you can create a cron trigger to generate an end-of-day sales report every weekday at 10pm.

To add a cron trigger, follow these steps:

Step 1: Navigate to Cron Triggers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Go to the ``Events`` tab in your Hasura console.
- Click ``Cron Triggers``.
- Click ``Create``.

.. thumbnail:: /img/graphql/manual/event-triggers/create-cron.png
   :alt: Adding a cron trigger
   :width: 1000px

Step 2: Define the cron trigger
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's define the cron trigger.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

      In the form opened by the above step, fill out the following fields:

      - **Name**: Create a name for the cron trigger.
      - **Webhook**: Enter the HTTP endpoint that should be triggered.
      - **Cron schedule**: Enter a schedule for the cron. You can use the link next to the field to help `build a cron expression <https://crontab.guru/#*_*_*_*_*>`__, or use the ``Frequently used crons`` dropdown as a shortcut. Cron events are created based on the UTC timezone.
      - **Payload**: The JSON payload which will be sent to the webhook.

      .. thumbnail:: /img/graphql/manual/event-triggers/define-cron-trigger.png
         :alt: Defining a cron trigger
         :width: 550px

      In this example, we're creating a cron trigger called ``eod_reports``, to trigger the webhook ``https://mywebhook.com/eod``. The cron schedule is set to ``0 22 * * 1-5``, which means "At 22:00 on every day-of-week from Monday through Friday" (you can check this `here <https://crontab.guru/#0_22_*_*_1-5>`__).

   .. tab:: CLI

      You can define a cron trigger by adding it to the ``cron_triggers.yaml`` file inside the ``metadata`` directory:

      .. code-block:: yaml

         - name: eod_reports
           webhook: https://mywebhook.com/eod
           schedule: 0 22 * * 1-5
           include_in_metadata: true
           payload: {}
      
      Apply the metadata by running:

      .. code-block:: yaml

         hasura metadata apply

   .. tab:: API

      You can define a cron trigger via the :ref:`create_cron_trigger metadata API <create_cron_trigger>`:

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type": "create_cron_trigger",
            "args": {
               "name": "eod_reports",
               "webhook": "https://mywebhook.com/eod",
               "schedule": "0 22 * * 1-5",
               "payload": {},
               "include_in_metadata": true
            }
         }

Step 3: Define advanced options (Optional)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you like, you can also define the following values:

- **Headers**: List of headers to be sent to the webhook.
- **Retry configuration**: In case the call to the webhook fails.
- **Include in metadata**: When set to true, the cron trigger will be included in the metadata and can be exported along with it.
- **Comment**: Custom description of the cron trigger.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

      Expand the ``Advanced`` section.

      .. thumbnail:: /img/graphql/manual/event-triggers/advanced-cron.png
         :alt: Defining advanced options for a cron trigger
         :width: 700px

   .. tab:: CLI

      You can define advanced options for a crone trigger when adding it to the ``cron_triggers.yaml`` file inside the ``metadata`` directory:

      .. code-block:: yaml

         - name: eod_reports
           webhook: https://mywebhook.com/eod
           schedule: 0 22 * * 1-5
           include_in_metadata: true
           payload: {}
           retry_conf:
             num_retries: 3
             timeout_seconds: 120
             tolerance_seconds: 21675
             retry_interval_seconds: 12
           comment: This is a comment

      Apply the metadata by running:

      .. code-block:: yaml

         hasura metadata apply

   .. tab:: API

      You can define advanced options for a cron trigger when defining it via the :ref:`create_cron_trigger metadata API <create_cron_trigger>`:

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type": "create_cron_trigger",
            "args": {
               "name": "eod_reports",
               "webhook": "https://mywebhook.com/eod",
               "schedule": "0 22 * * 1-5",
               "include_in_metadata": true,
               "payload": {},
               "retry_conf": {
                     "num_retries": 3,
                     "timeout_seconds": 120,
                     "tolerance_seconds": 21675,
                     "retry_interval_seconds": 12
               },
               "comment": "sample_cron commment"
            }
         }

Schedule & logs
^^^^^^^^^^^^^^^

Once you've created your cron trigger, you can see ``Pending events``, ``Processed events``, and ``Invocation logs`` in their respective tabs.

.. thumbnail:: /img/graphql/manual/event-triggers/pending-cron.png
   :alt: Schedule and logs for cron triggers
   :width: 1200px

One-off scheduled events
------------------------

One-off scheduled events are used to reliably trigger an HTTP webhook to run custom business logic at a particular point in time. For example, you can create a scheduled event to send a reminder email two weeks after a user signs up.

To add a one-off scheduled event, follow these steps:

Step 1: Navigate to One-off Scheduled Events
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Go to the ``Events`` tab in your Hasura console.
- Click ``One-off Scheduled Events``.
- Click ``Schedule an event``.

.. thumbnail:: /img/graphql/manual/event-triggers/one-off.png
   :alt: Adding a one-off scheduled event
   :width: 900px

Step 2: Define the scheduled event
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Define the following values for a scheduled event:

- **Webhook**: Enter the HTTP endpoint that should be triggered.
- **Time**: Enter the time to trigger the event.
- **Payload**: The JSON payload which will be sent to the webhook.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

      In the form opened by the above step, fill out the following fields:

      .. thumbnail:: /img/graphql/manual/event-triggers/define-one-off-event.png
         :alt: Defining the scheduled event
         :width: 550px

   .. tab:: API

      You can define a scheduled event via the :ref:`create_scheduled_event metadata API <create_scheduled_event>`:

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type": "create_scheduled_event",
            "args": {
               "webhook": "https://send-email.com",
               "schedule_at": "2022-06-18T18:45:00Z",
               "payload": { "email": "bob@ross.com" }
            }
         }

Step 3: Define advanced options (Optional)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you like, you can also define advanced values:

- **Headers**: List of headers to be sent to the webhook.
- **Retry configuration**: In case the call to the webhook fails.
- **Comment**: Custom description of the scheduled trigger.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

      Expand the ``Advanced`` section.

      .. thumbnail:: /img/graphql/manual/event-triggers/advanced-one-off.png
         :alt: Defining advanced options for a scheduled event
         :width: 700px

   .. tab:: API

      You can define advanced options when defining a scheduled event via the :ref:`create_scheduled_event metadata API <create_scheduled_event>`:

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type": "create_scheduled_event",
            "args": {
               "webhook": "https://send-email.com",
               "schedule_at": "2022-06-18T18:45:00Z",
               "payload": {
                     "email": "bob@ross.com"
               },
               "headers": [
                     {
                        "name": "key",
                        "value": "value"
                     }
               ],
               "retry_conf": {
                     "num_retries": 3,
                     "timeout_seconds": 120,
                     "tolerance_seconds": 21675,
                     "retry_interval_seconds": 12
               },
               "comment": "sample scheduled event comment"
            }
         }

Schedule & logs
^^^^^^^^^^^^^^^

Once you've created your scheduled trigger, you can see ``Pending events``, ``Processed events``, and ``Invocation logs`` in their respective tabs.

.. thumbnail:: /img/graphql/manual/event-triggers/pending-one-off.png
   :alt: Schedule and logs for scheduled events
   :width: 1200px
