Event trigger payload
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The following is the payload and delivery mechanism of an event to the webhook when an event trigger is invoked.

HTTP request method
-------------------
Delivered over ``HTTP POST`` with the following headers:

.. code-block:: none

   Content-Type: application/json

JSON payload
------------

.. code-block:: none

    {
      "event": {
          "session_variables": <session-variables>,
          "op": "<op-name>",
          "data": {
              "old": <column-values>,
              "new": <column-values>
          }
      },
      "created_at": "<timestamp>",
      "id": "<uuid>",
      "trigger": {
          "name": "<name-of-trigger>"
      },
      "table":  {
          "schema": "<schema-name>",
          "name": "<table-name>"
      }
    }


.. list-table::
   :header-rows: 1

   * - Key
     - Type
     - Description
   * - session-variables
     - Object_ or NULL
     - Key-value pairs of session variables (i.e. "x-hasura-\*" variables) and their values. NULL if no session variables found.
   * - op-name
     - OpName_
     - Name of the operation. Can only be "INSERT", "UPDATE", "DELETE", "MANUAL"
   * - column-values
     - Object_
     - Key-value pairs of column name and their values of the table
   * - timestamp
     - String
     - Timestamp at which event was created
   * - uuid
     - String
     - UUID identifier for the event
   * - name-of-trigger
     - String
     - Name of the trigger
   * - schema-name
     - String
     - Name of the schema for the table
   * - table-name
     - String
     - Name of the table


**In case of**:

- INSERT

  - ``event.data.old`` will be ``null``
  - ``event.data.new`` will contain the insert row

- UPDATE

  - ``event.data.old`` will be values before the update
  - ``event.data.new`` will contain the values after the update

- DELETE

  - ``event.data.old`` will contain the row that is deleted
  - ``event.data.new`` will be ``null``

- MANUAL

  - ``event.data.old`` will be ``null``
  - ``event.data.new`` will contain the current row

**For example**:

.. code-block:: json

    {
      "id": "85558393-c75d-4d2f-9c15-e80591b83894",
      "created_at": "2018-09-05T07:14:21.601701Z",
      "trigger": {
          "name": "test_trigger"
      },
      "table": {
          "schema": "public",
          "name": "users"
      },
      "event": {
          "session_variables": {
              "x-hasura-role": "admin",
              "x-hasura-allowed-roles": "['user', 'boo', 'admin']",
              "x-hasura-user-id": "1"
          },
          "op": "INSERT",
          "data": {
            "old": null,
            "new": {
                "id":"42",
                "name": "john doe"
            }
          }
      }
    }



Syntax definitions
------------------

Object
^^^^^^

.. code-block:: none

  {
    "column1": "value1",
    "column2": "value2",
    ..
  }


OpName
^^^^^^

.. parsed-literal::

   "INSERT" | "UPDATE" | "DELETE" | "MANUAL"

Webhook response structure
--------------------------

A ``2xx`` response status code is deemed to be a successful invocation of the webhook. Any other response status will be
deemed as an unsuccessful invocation which will cause retries as per the retry configuration.

It is also recommended that you return a JSON object in your webhook response.

Retry-After header
^^^^^^^^^^^^^^^^^^

If the webhook response contains a ``Retry-After`` header, then the event will be redelivered once more after the duration (in seconds) found in the header. Note that the header will be respected only if the response status code is ``non-2xx``.

The ``Retry-After`` header can be used for retrying/rate-limiting/debouncing your webhook triggers.
