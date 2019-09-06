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
           "table": "users",
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
     - :ref:`TableName <TableName>`
     - Name of the table
   * - webhook
     - true
     - String
     - Full url of webhook
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
     - [ HeaderFromValue_ | HeaderFromEnv_ ]
     - List of headers to be sent with the webhook
   * - replace
     - false
     - Boolean
     - If set to true, event trigger is replaced with the new definition

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

.. _invoke_event_trigger:

invoke_event_trigger
--------------------

``invoke_event_trigger`` is used to invoke an event trigger manually.

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
     - List of columns or "*" to listen changes on
   * - payload
     - false
     - EventTriggerColumns_
     - List of columns or "*" to send as part of webhook payload

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

.. _EventTriggerColumns:

EventTriggerColumns
&&&&&&&&&&&&&&&&&&&

.. parsed-literal::
   :class: haskell-pre

   "*" | [:ref:`PGColumn`]


