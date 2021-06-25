.. meta::
   :description: Manage scheduled triggers with the Hasura schema/metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, scheduled trigger

Schema/Metadata API Reference: Scheduled Triggers
=================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Scheduled triggers are used to invoke webhooks based on a timestamp or cron.

.. admonition:: Supported from

  Scheduled triggers are supported from versions ``v1.3.0`` and above.

.. admonition:: Deprecation

  In versions ``v2.0.0`` and above, the schema/metadata API is deprecated in favour of the :ref:`schema API <schema_apis>` and the
  :ref:`metadata API <metadata_apis>`.

  Though for backwards compatibility, the schema/metadata APIs will continue to function.

.. _create_cron_trigger:

create_cron_trigger
-------------------

``create_cron_trigger`` is used to create a new cron trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "create_cron_trigger",
       "args" : {
           "name": "sample_cron",
           "webhook": "https://httpbin.org/post",
           "schedule":  "* * * * *",
           "payload": {
               "key1": "value1",
               "key2": "value2"
           },
           "include_in_metadata":false,
           "comment":"sample_cron commment"
       }
   }

.. _create_cron_trigger_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`TriggerName <TriggerName>`
     - Name of the cron trigger
   * - webhook
     - true
     - :ref:`WebhookURL <WebhookURL>`
     - URL of the webhook
   * - schedule
     - true
     - Cron Expression
     - Cron expression at which the trigger should be invoked.
   * - payload
     - false
     - JSON
     - Any JSON payload which will be sent when the webhook is invoked.
   * - headers
     - false
     - [ :ref:`HeaderFromValue <HeaderFromValue>` | :ref:`HeaderFromEnv <HeaderFromEnv>` ]
     - List of headers to be sent with the webhook
   * - retry_conf
     - false
     - :ref:`RetryConfST`
     - Retry configuration if scheduled invocation delivery fails
   * - include_in_metadata
     - true
     - Boolean
     - Flag to indicate whether a trigger should be included in the metadata. When a cron
       trigger is included in the metadata, the user will be able to export it when the
       metadata of the graphql-engine is exported.
   * - comment
     - false
     - Text
     - Custom comment.
   * - replace
     - false
     - Bool
     - When replace is set to ``true``, the cron trigger will be updated(if exists) and when it's ``false`` or the
       field is omitted, then a new cron trigger will be created.


.. admonition:: Supported from

  Scheduled triggers are supported from versions ``v1.3.0`` and above.

.. _delete_cron_trigger:

delete_cron_trigger
-------------------

``delete_cron_trigger`` is used to delete an existing cron trigger. The scheduled events associated with the cron trigger will also be deleted.


.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "delete_cron_trigger",
       "args" : {
           "name": "sample_cron"
       }
   }

.. _delete_cron_trigger_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`TriggerName <TriggerName>`
     - Name of the cron trigger

.. admonition:: Supported from

  Scheduled triggers are supported from versions ``v1.3.0`` and above.

.. _create_scheduled_event:

create_scheduled_event
----------------------

``create_scheduled_event`` is used to create a scheduled event.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "create_scheduled_event",
       "args" : {
           "webhook": "https://httpbin.org/post",
           "schedule_at": "2019-09-09T22:00:00Z",
           "payload": {
               "key1": "value1",
               "key2": "value2"
           },
           "headers" : [{
               "name":"header-key",
               "value":"header-value"
           }],
           "comment":"sample scheduled event comment"
       }
   }

.. _create_scheduled_event_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - webhook
     - true
     - :ref:`WebhookURL <WebhookURL>`
     - URL of the webhook
   * - schedule_at
     - true
     - Timestamp (ISO8601 format)
     - The time at which the invocation should be invoked.
   * - payload
     - false
     - JSON
     - Any JSON payload which will be sent when the webhook is invoked.
   * - headers
     - false
     - [ :ref:`HeaderFromValue <HeaderFromValue>` | :ref:`HeaderFromEnv <HeaderFromEnv>` ]
     - List of headers to be sent with the webhook
   * - retry_conf
     - false
     - :ref:`RetryConfST`
     - Retry configuration if scheduled event delivery fails
   * - comment
     - false
     - Text
     - Custom comment.


.. admonition:: Supported from

  Scheduled triggers are supported from versions ``v1.3.0`` and above.