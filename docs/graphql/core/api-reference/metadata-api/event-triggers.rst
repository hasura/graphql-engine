.. meta::
   :description: Manage event triggers with the Hasura metadata API
   :keywords: hasura, docs, metadata API, API reference, event trigger

.. _metadata_api_event_triggers:

Metadata API Reference: Event Triggers (v2.0 and above)
=======================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Event triggers are used to capture database changes and send them to a configured webhook.

.. admonition:: Supported from

  The metadata API is supported for versions ``v2.0.0`` and above and replaces the older
  :ref:`schema/metadata API <schema_metadata_apis>`.

.. _pg_create_event_trigger:

pg_create_event_trigger
-----------------------

``pg_create_event_trigger`` is used to create a new event trigger or replace an existing event trigger.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_create_event_trigger",
       "args" : {
           "name": "sample_trigger",
           "table": {
              "name": "users",
              "schema": "public"
           },
           "source": "default",
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

.. _pg_create_event_trigger_syntax:

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
     - Name of the event trigger
   * - table
     - true
     - :ref:`QualifiedTable <QualifiedTable>`
     - Object with table name and schema
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)
   * - webhook
     - false
     - String
     - Full url of webhook (*)
   * - webhook_from_env
     - false
     - String
     - Environment variable name of webhook (must exist at boot time) (*)
   * - insert
     - false
     - :ref:`OperationSpec`
     - Specification for insert operation
   * - update
     - false
     - :ref:`OperationSpec`
     - Specification for update operation
   * - delete
     - false
     - :ref:`OperationSpec`
     - Specification for delete operation
   * - headers
     - false
     - [ :ref:`HeaderFromValue <HeaderFromValue>` | :ref:`HeaderFromEnv <HeaderFromEnv>` ]
     - List of headers to be sent with the webhook
   * - retry_conf
     - false
     - :ref:`RetryConf`
     - Retry configuration if event delivery fails
   * - replace
     - false
     - Boolean
     - If set to true, the event trigger is replaced with the new definition
   * - enable_manual
     - false
     - Boolean
     - If set to true, the event trigger can be invoked manually 

(*) Either ``webhook`` or ``webhook_from_env`` are required.

.. _pg_delete_event_trigger:

pg_delete_event_trigger
-----------------------

``pg_delete_event_trigger`` is used to delete an event trigger.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_delete_event_trigger",
       "args" : {
           "name": "sample_trigger",
           "source": "default"
       }
   }

.. _pg_delete_event_trigger_syntax:

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
     - Name of the event trigger
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the trigger (default: ``default``)

.. _pg_redeliver_event:

pg_redeliver_event
------------------

``redeliver_event`` is used to redeliver an existing event. For example, if an event is marked as error (
say it did not succeed after retries), you can redeliver it using this API. Note that this will reset the count of retries so far.
If the event fails to deliver, it will be retried automatically according to its ``retry_conf``.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_redeliver_event",
       "args" : {
           "event_id": "ad4f698f-a14e-4a6d-a01b-38cd252dd8bf"
       }
   }

.. _pg_redeliver_event_syntax:

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


.. _pg_invoke_event_trigger:

pg_invoke_event_trigger
-----------------------

``invoke_event_trigger`` is used to invoke an event trigger with custom payload.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_invoke_event_trigger",
       "args" : {
           "name": "sample_trigger",
           "source": "default",
           "payload": {}
       }
   }

.. _pg_invoke_event_trigger_syntax:

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
     - Name of the event trigger
   * - payload
     - true
     - JSON
     - Some JSON payload to send to trigger
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the trigger (default: ``default``)
