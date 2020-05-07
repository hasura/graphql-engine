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
     - Retry configuration if scheduled invocation delivery fails
   * - include_in_metadata
     - false
     - Boolean
     - Flag to indicate whether a trigger should be included in the metadata. When a trigger is included in the metadata, the user will be able to export it when the metadata of the graphql-engine is
       exported.
   * - comment
     - false
     - Text
     - Custom comment.

.. _update_cron_trigger:

update_cron_trigger
-------------------

``update_cron_trigger`` is used to update an existing cron trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "update_cron_trigger",
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

.. _update_cron_trigger_syntax:

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
     - Text | UrlFromEnv_
     - URL of webhook or environment variable which has the URL
   * - schedule
     - true
     - Cron Expression
     - Cron expression at which the trigger should be invoked.
   * - payload
     - false
     - JSON
     - Any JSON payload which will be sent with the scheduled invocation
   * - headers
     - false
     - [ HeaderFromValue_ | HeaderFromEnv_ ]
     - List of headers to be sent with the webhook
   * - retry_conf
     - false
     - RetryConfST_
     - Retry configuration if scheduled invocation delivery fails
   * - include_in_metadata
     - false
     - Boolean
     - Flag to indicate whether a trigger should be included in the metadata. When a trigger is included in the metadata, the user will be able to export it when the metadata of the graphql-engine is
       exported.
   * - comment
     - false
     - Text
     - Custom comment.


.. _delete_cron_trigger:

delete_cron_trigger
-------------------

``delete_cron_trigger`` is used to delete an existing cron trigger.
The scheduled invocations associated with the cron trigger will also be deleted.

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

.. _fetch_cron_trigger_invocations:

fetch_cron_trigger_invocations
------------------------------

``fetch_cron_trigger_invocations`` is used to fetch scheduled invocations of an existing cron trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "fetch_cron_trigger_invocations",
       "args" : {
           "name": "sample_cron",
           "limit": 100,
           "offset": 10,
       }
   }

.. _fetch_cron_trigger_invocations_syntax:

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
     - Name of the trigger
   * - limit
     - false
     - Integer
     - Maximum number of invocations to be returned.
   * - offset
     - false
     - Integer
     - The starting offset of the scheduled invocations to be returned in the API call to be returned.

.. _create_one_off_trigger:

create_one_off_trigger
----------------------

``create_scheduled_trigger_cron_one_off`` is used to create an one-off trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "create_one_off_trigger",
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
           "comment":"sample one-off trigger commment"
       }
   }

.. _create_one_off_trigger_syntax:

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

.. _fetch_one_off_trigger_invocations:

fetch_one_off_trigger_invocations
---------------------------------

``fetch_one_off_trigger_invocations`` is used to fetch the one-off triggers with their invocations.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "fetch_one_off_trigger_invocations",
       "args" : {
           "limit": 100,
           "offset": 10
       }
   }

.. _fetch_one_off_trigger_invocations_syntax:

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
     - Maximum number of one-off triggers to be returned.
   * - offset
     - false
     - Integer
     - The starting offset of the scheduled one-off triggers to be returned.

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
