.. meta::
   :description: Manage event triggers with the Hasura schema/metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, event trigger

.. _api_event_triggers:

Schema/Metadata API Reference: Event Triggers
=============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Event triggers are used to capture database changes and send them to a configured webhook.

.. _create_event_trigger:

create_event_trigger
--------------------

``create_event_trigger`` is used to create a new event trigger or replace an existing event trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "create_event_trigger",
       "args" : {
           "name": "sample_trigger",
           "table": {
              "name": "users",
              "schema": "public",
           },
           "webhook": "https://httpbin.org/post",
           "insert": {
               "columns": "*",
               "payload": ["username"]
           },
           "update": {
               "columns": ["username", "real_name"],
               "payload": "*"
           },
           "delete": {
               "columns": "*"
           },
           "headers":[
             {
                 "name": "X-Hasura-From-Val",
                 "value": "myvalue"
             },
             {
                 "name": "X-Hasura-From-Env",
                 "value_from_env": "EVENT_WEBHOOK_HEADER"
             }
           ],
           "replace": false
       }
   }

.. _create_event_trigger_syntax:

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
     - Name of the event trigger
   * - table
     - true
     - :ref:`QualifiedTable <QualifiedTable>`
     - Object with table name and schema
   * - webhook
     - false
     - String
     - Full url of webhook (*)
   * - webhook_from_env
     - false
     - String
     - Environment variable name of webhook (must exists at boot time) (*)
   * - insert
     - false
     - OperationSpec_
     - Specification for insert operation
   * - update
     - false
     - OperationSpec_
     - Specification for update operation
   * - delete
     - false
     - OperationSpec_
     - Specification for delete operation
   * - headers
     - false
     - [ :ref:`HeaderFromValue <HeaderFromValue>` | :ref:`HeaderFromEnv <HeaderFromEnv>` ]
     - List of headers to be sent with the webhook
   * - replace
     - false
     - Boolean
     - If set to true, the event trigger is replaced with the new definition

(*) Either `webhook` or `webhook_from_env` are required.

.. _delete_event_trigger:

delete_event_trigger
--------------------

``delete_event_trigger`` is used to delete an event trigger.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "delete_event_trigger",
       "args" : {
           "name": "sample_trigger"
       }
   }

.. _delete_event_trigger_syntax:

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
     - Name of the event trigger


.. _redeliver_event:

redeliver_event
---------------

``redeliver_event`` is used to redeliver an existing event. For example, if an event is marked as error (
say it did not succeed after retries), you can redeliver it using this API. Note that this will reset the count of retries so far.
If the event fails to deliver, it will be retried automatically according to its ``retry_conf``.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "redeliver_event",
       "args" : {
           "event_id": "ad4f698f-a14e-4a6d-a01b-38cd252dd8bf"
       }
   }

.. _redeliver_event_syntax:

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
     - String
     - UUID of the event


.. _invoke_event_trigger:

invoke_event_trigger
--------------------

``invoke_event_trigger`` is used to invoke an event trigger with custom payload.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "invoke_event_trigger",
       "args" : {
           "name": "sample_trigger",
           "payload": {}
       }
   }

.. _invoke_event_trigger_syntax:

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
     - Name of the event trigger
   * - payload
     - true
     - JSON
     - Some JSON payload to send to trigger

.. _TriggerName:

TriggerName
&&&&&&&&&&&

.. parsed-literal::

  String

.. _OperationSpec:

OperationSpec
&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - columns
     - true
     - EventTriggerColumns_
     - List of columns or "*" to listen to changes
   * - payload
     - false
     - EventTriggerColumns_
     - List of columns or "*" to send as part of webhook payload

.. _EventTriggerColumns:

EventTriggerColumns
&&&&&&&&&&&&&&&&&&&

.. parsed-literal::
   :class: haskell-pre

   "*" | [:ref:`PGColumn`]
