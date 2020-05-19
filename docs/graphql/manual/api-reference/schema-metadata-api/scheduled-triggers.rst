.. meta::
   :description: Manage scheduled triggers with the Hasura schema/metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, scheduled trigger

Schema/Metadata API Reference: Scheduled Triggers
=================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Scheduled triggers are used to invoke webhooks based on a timestamp or cron.

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
     - TriggerName_
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
     - [ HeaderFromValue_ | HeaderFromEnv_ ]
     - List of headers to be sent with the webhook
   * - retry_conf
     - false
     - RetryConfST_
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
     - TriggerName_
     - Name of the cron trigger

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
           "headers" : {
               "name":"header-key",
               "value":"header-value"
           },
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
     - [ HeaderFromValue_ | HeaderFromEnv_ ]
     - List of headers to be sent with the webhook
   * - retry_conf
     - false
     - RetryConfST_
     - Retry configuration if scheduled event delivery fails
   * - comment
     - false
     - Text
     - Custom comment.

.. _TriggerName:

TriggerName
&&&&&&&&&&&

.. parsed-literal::

  String

.. _UrlFromEnv:

UrlFromEnv
&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - from_env
     - true
     - String
     - Name of the environment variable which has the URL

.. _HeaderFromValue:

HeaderFromValue
&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - name
     - true
     - String
     - Name of the header
   * - value
     - true
     - String
     - Value of the header

.. _HeaderFromEnv:

HeaderFromEnv
&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - name
     - true
     - String
     - Name of the header
   * - value_from_env
     - true
     - String
     - Name of the environment variable which holds the value of the header

.. _RetryConfST:

RetryConfST
&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - num_retries
     - false
     - Integer
     - Number of times to retry delivery. Default: 0
   * - retry_interval_seconds
     - false
     - Integer
     - Number of seconds to wait between each retry. Default: 10
   * - timeout_seconds
     - false
     - Integer
     - Number of seconds to wait for response before timing out. Default: 60
   * - tolerance_seconds
     - false
     - Integer
     - Number of seconds between scheduled time and actual delivery time that is acceptable. If the time difference is more than this, then the event is dropped. Default: 21600 (6 hours)
