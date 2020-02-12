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
               "type": "Cron",
               "value": "* * * * *"
           },
           "payload": {
               "key1": "value1",
               "key2": "value2"
           }
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
     - false
     - String
     - Full url of webhook
   * - webhook_from_env
     - false
     - String
     - Environment variable which has the full url of webhook
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


.. _update_scheduled_trigger:

update_scheduled_trigger
------------------------

``update_scheduled_trigger`` is used to update an existing scheduled trigger.

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
     - false
     - String
     - Full url of webhook
   * - webhook_from_env
     - false
     - String
     - Environment variable which has the full url of webhook
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
     - Id of the scheduled event
 
.. _TriggerName:

TriggerName
&&&&&&&&&&&

.. parsed-literal::

  String

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
     - OneOff | Cron
     - Type of scheduled trigger
   * - value
     - true
     - String
     - Timestamp in UTC or cron expression

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
   * - interval_sec
     - false
     - Integer
     - Number of seconds to wait between each retry. Default: 10
   * - timeout_sec
     - false
     - Integer
     - Number of seconds to wait for response before timing out. Default: 60
   * - tolerance
     - false
     - Integer
     - Number of minutes between scheduled time and actual delivery time that is acceptable. If the time difference is more than this, then the event is dropped. Default: 360 (6 hours)


