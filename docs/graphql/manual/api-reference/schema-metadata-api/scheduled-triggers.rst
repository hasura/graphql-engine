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

.. _create_scheduled_trigger:

create_scheduled_trigger
------------------------

``create_scheduled_trigger`` is used to create a new scheduled trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "create_scheduled_trigger",
       "args" : {
           "name": "sample_cron",
           "webhook": "https://httpbin.org/post",
           "schedule": {
               "type": "cron",
               "value": "* * * * *"
           },
           "payload": {
               "key1": "value1",
               "key2": "value2"
           },
           "include_in_metadata":false
       }

   }

.. _create_scheduled_trigger_syntax:

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
     - Name of the scheduled trigger
   * - webhook
     - true
     - Text | UrlFromEnv_
     - URL of webhook or environment variable which has the URL
   * - schedule
     - true
     - ScheduleConf_
     - Schedule configuration
   * - payload
     - false
     - JSON
     - Any JSON payload which will be sent with the scheduled event
   * - headers
     - false
     - [ HeaderFromValue_ | HeaderFromEnv_ ]
     - List of headers to be sent with the webhook
   * - retry_conf
     - false
     - RetryConfST_
     - Retry configuration if scheduled event delivery fails
   * - include_in_metadata
     - false
     - Boolean
     - Flag to indicate whether a scheduled trigger should be included in the metadata. When a scheduled trigger is included in the metadata, the user will be able to export it when the metadata of the graphql-engine is
       exported.

.. _update_scheduled_trigger:

update_scheduled_trigger
------------------------

``update_scheduled_trigger`` is used to update an existing scheduled trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "update_scheduled_trigger",
       "args" : {
           "name": "sample_cron",
           "webhook": "https://httpbin.org/post",
           "schedule": {
               "type": "cron",
               "value": "* * * * *"
           },
           "payload": {
               "key1": "value1",
               "key2": "value2"
           },
           "include_in_metadata":false
       }

   }

.. _update_scheduled_trigger_syntax:

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
     - Name of the scheduled trigger
   * - webhook
     - true
     - Text | UrlFromEnv_
     - URL of webhook or environment variable which has the URL
   * - schedule
     - true
     - ScheduleConf_
     - Schedule configuration
   * - payload
     - false
     - JSON
     - Any JSON payload which will be sent with the scheduled event
   * - headers
     - false
     - [ HeaderFromValue_ | HeaderFromEnv_ ]
     - List of headers to be sent with the webhook
   * - retry_conf
     - false
     - RetryConfST_
     - Retry configuration if scheduled event delivery fails
   * - include_in_metadata
     - false
     - Boolean
     - Flag to indicate whether a scheduled trigger should be included in the metadata. When a scheduled trigger is included in the metadata, the user will be able to export it when the metadata of the graphql-engine is
       exported.

.. _delete_scheduled_trigger:

delete_scheduled_trigger
------------------------

``delete_scheduled_trigger`` is used to delete an existing scheduled trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "delete_scheduled_trigger",
       "args" : {
           "name": "sample_cron"
       }
   }

.. _delete_scheduled_trigger_syntax:

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
     - Name of the scheduled trigger

.. _invoke_scheduled_trigger:

invoke_scheduled_trigger
----------------------

``invoke_scheduled_trigger`` is used to create new invocations of an existing scheduled trigger
at the given timestamp along with an optional payload. This API should be used to create new
invocations of an adhoc scheduled trigger.

When the payload is provided, it will
override the configured payload (the payload with which the scheduled trigger was created).
When the payload is not provided, the configured payload will be used.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "invoke_scheduled_trigger",
       "args" : {
           "name": "sample-adhoc",
           "timestamp": "2020-02-14 22:00:00 Z",
           "payload": { "k" : "v"}
       }
   }

.. _invoke_scheduled_trigger_syntax:

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
     - Text
     - Name of the scheduled trigger
   * - timestamp
     - true
     - UTCTime
     - UTC Timestamp to invoke the trigger in ISO8601 format
   * - payload
     - false
     - JSON
     - Any JSON object to send with the scheduled trigger, will override configured payload

.. _cancel_scheduled_event:

cancel_scheduled_event
----------------------

``cancel_scheduled_event`` is used to cancel a particular run of a scheduled trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "cancel_scheduled_event",
       "args" : {
           "event_id": "237b604c-67f1-4aa8-8453-36855cfebfc4"
       }
   }

.. _cancel_scheduled_event_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - event_id
     - true
     - UUID
     - ID of the scheduled event

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

.. _ScheduleConf:

ScheduleConf
&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - type
     - true
     - ``cron`` | ``adhoc``
     - Type of scheduled trigger
   * - value
     - true (when type is ``cron``).
       false (when type is ``adhoc``)
     - String
     - When the type is ``cron``, then a cron expression is expected
       . There is no value expected when the type is ``adhoc``.


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
