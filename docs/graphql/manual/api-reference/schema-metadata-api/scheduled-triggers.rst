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

.. _create_scheduled_trigger_cron:

create_scheduled_trigger_cron
-----------------------------

``create_scheduled_trigger_cron`` is used to create a new cron scheduled trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "create_scheduled_trigger_cron",
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

.. _create_scheduled_trigger_cron_syntax:

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
     - Retry configuration if scheduled event delivery fails
   * - include_in_metadata
     - false
     - Boolean
     - Flag to indicate whether a scheduled trigger should be included in the metadata. When a scheduled trigger is included in the metadata, the user will be able to export it when the metadata of the graphql-engine is
       exported.
   * - comment
     - false
     - Text
     - Custom comment.

.. _update_scheduled_trigger_cron:

update_scheduled_trigger_cron
-----------------------------

``update_scheduled_trigger_cron`` is used to update an existing cron scheduled trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "update_scheduled_trigger_cron",
       "args" : {
           "name": "sample_cron",
           "webhook": "https://httpbin.org/post",
           "schedule":"* * * *",
           "payload": {
               "key1": "value1",
               "key2": "value2"
           }
       }

   }

.. _update_scheduled_trigger_cron_syntax:

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
     - Cron Expression
     - Cron expression at which the trigger should be invoked.
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
   * - comment
     - false
     - Text
     - Custom comment.


.. _delete_scheduled_trigger_cron:

delete_scheduled_trigger_cron
-----------------------------

``delete_scheduled_trigger_cron`` is used to delete an existing cron scheduled trigger.The scheduled events associated with the  cron scheduled trigger will also be deleted.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "delete_scheduled_trigger_cron",
       "args" : {
           "name": "sample_cron"
       }
   }

.. _delete_scheduled_trigger_cron_syntax:

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

.. _fetch_scheduled_trigger_cron_events:

fetch_scheduled_trigger_cron_events
-----------------------------------

``fetch_scheduled_trigger_cron_events`` is used to fetch scheduled events of an existing scheduled trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "fetch_scheduled_trigger_cron_events",
       "args" : {
           "name": "sample_cron",
           "limit": 100,
           "offset": 10,
       }
   }

.. _fetch_scheduled_trigger_cron_events_syntax:

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
   * - limit
     - false
     - Integer
     - Maximum number of scheduled events to be returned.
   * - offset
     - false
     - Integer
     - The starting offset of the scheduled events to be returned in the API call to be returned.

.. _create_scheduled_trigger_one_off:

create_scheduled_trigger_one_off
--------------------------------

``create_scheduled_trigger_cron_one_off`` is used to create an one-off scheduled trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "create_scheduled_trigger_one_off",
       "args" : {
           "webhook": "https://httpbin.org/post",
           "schedule_at": "2019-09-09T22:00:00Z",
           "payload": {
               "key1": "value1",
               "key2": "value2"
           },
           "headers" : {
               "key":"header-key",
               "value":"header-value"
           },
           "comment":"sample one-off scheduled trigger commment"
       }
   }

.. _create_scheduled_trigger_one_off_syntax:

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
     - Text | UrlFromEnv_
     - URL of webhook or environment variable which has the URL
   * - schedule_at
     - true
     - Timestamp (ISO8601 format)
     - The time at which the event should be delivered.
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

.. _fetch_scheduled_trigger_one_off:

fetch_scheduled_trigger_one_off
-------------------------------

``fetch_scheduled_trigger_one_off`` is used to fetch the scheduled one-off triggers.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "fetch_scheduled_trigger_one_off",
       "args" : {
           "limit": 100,
           "offset": 10
       }
   }

.. _fetch_scheduled_trigger_one_off_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - limit
     - false
     - Integer
     - Maximum number of one-off scheduled triggers to be returned.
   * - offset
     - false
     - Integer
     - The starting offset of the scheduled one-off triggers to be returned.

.. _create_scheduled_trigger_one_off_template:

create_scheduled_trigger_one_off_template
-----------------------------------------

``create_scheduled_trigger_cron_one_off_template`` is used to create an one-off scheduled trigger template.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "create_scheduled_trigger_one_off_template",
       "args" : {
           "name" : "sample_one_off_template",
           "webhook": "https://httpbin.org/post",
           "payload": {
               "key1": "value1",
               "key2": "value2"
           },
           "headers" : {
               "key":"header-key",
               "value":"header-value"
           },
           "include_in_metadata":true,
           "comment":"sample one-off template scheduled trigger commment"
       }
   }

.. _create_scheduled_trigger_one_off_template_syntax:

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
     - Name of the one-off scheduled trigger template
   * - webhook
     - true
     - Text | UrlFromEnv_
     - URL of webhook or environment variable which has the URL
   * - payload
     - true
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
   * - include_in_metadata
     - false
     - Boolean
     - Flag to indicate whether a scheduled trigger should be included in the metadata. When a scheduled trigger is included in the metadata, the user will be able to export it when the metadata of the graphql-engine is
       exported.
   * - comment
     - false
     - Text
     - Custom comment.

.. _update_scheduled_trigger_one_off_template:

update_scheduled_trigger_one_off_template
-----------------------------------------

``update_scheduled_trigger_one_off_template`` is used to update an existing cron scheduled trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "update_scheduled_trigger_one_off_template",
       "args" : {
           "name": "sample_one_off_template",
           "webhook": "https://httpbin.org/post",
           "schedule":"* * * *",
           "payload": {
               "key1": "value1",
               "key2": "value2"
           }
       }

   }

.. _update_scheduled_trigger_one_off_template_syntax:

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
     - Cron Expression
     - Cron expression at which the trigger should be invoked.
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
   * - comment
     - false
     - Text
     - Custom comment.

.. _invoke_scheduled_trigger_one_off_template:

invoke_scheduled_trigger_one_off_template
-----------------------------------------

``invoke_scheduled_trigger_one_off_template`` is used to create a new invocation of an existing
one-off scheduled trigger template
at the given timestamp along with an optional payload.

When the payload is provided, it will
override the configured payload (the payload with which the one-off template was created).
When the payload is not provided, the configured payload will be used.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "invoke_scheduled_trigger_one_off_template",
       "args" : {
           "name": "sample_one_off_template",
           "schedule_at": "2020-02-14 22:00:00 Z",
           "payload": { "k" : "v"}
       }
   }

.. _invoke_scheduled_trigger_one_off_template_syntax:

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
   * - schedule_at
     - true
     - UTCTime
     - UTC Timestamp to invoke the trigger in ISO8601 format
   * - payload
     - false
     - JSON
     - Any JSON object to send with the scheduled trigger, will override configured payload

.. _delete_scheduled_trigger_one_off_template:

delete_scheduled_trigger_one_off_template
-----------------------------

``delete_scheduled_trigger_one_off_template`` is used to delete an existing one-off scheduled trigger template.
The scheduled events associated with the one-off template will also be deleted.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "delete_scheduled_trigger_one_off_template",
       "args" : {
           "name": "sample_one_off_template"
       }
   }

.. _delete_scheduled_trigger_one_off_template_syntax:

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


.. _fetch_scheduled_trigger_one_off_template_events:

fetch_scheduled_trigger_one_off_template_events
-----------------------------

``fetch_scheduled_trigger_one_off_template_events`` is used to fetch scheduled events of an existing scheduled trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "fetch_scheduled_trigger_one_off_template_events",
       "args" : {
           "name": "sample_one_off_template",
           "limit": 100,
           "offset": 10
       }
   }

.. _fetch_scheduled_trigger_one_off_template_events_syntax:

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
   * - limit
     - false
     - Integer
     - Maximum number of scheduled events to be returned.
   * - offset
     - false
     - Integer
     - The starting offset of the scheduled events to be returned in the API call to be returned.

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
