Event trigger payload
=====================

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
          "op": "<op-name>",
          "data": {
            "old": <column-values>,
            "new": <column-values>
          }
      },
      "created_at": "<timestamp>",
      "id": "<uuid>",
      "trigger": {
          "name": "<name-of-trigger>",
          "id": "<uuid>"
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
   * - op-name
     - OpName_
     - Name of the operation. Can only be "INSERT", "UPDATE" or "DELETE"
   * - column-values
     - Object_
     - Key-value pairs of column name and their values of the table
   * - timestamp
     - String
     - Timestamp value
   * - uuid
     - String
     - A UUID value
   * - name-of-trigger
     - String
     - Name of the trigger
   * - schema-name
     - String
     - Name of the postgres schema where the table is
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

**For example**:

.. code-block:: json

    {
      "id": "85558393-c75d-4d2f-9c15-e80591b83894",
      "created_at": "2018-09-05T07:14:21.601701Z",
      "trigger": {
          "name": "test_trigger",
          "id": "37b7f91a-b3a5-4b85-be59-e5920d72f6aa"
      },
      "table": {
          "schema": "public",
          "name": "users"
      },
      "event": {
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

.. _Object:

Object
^^^^^^

.. code-block:: none

  {
    "column1": "value1",
    "column2": "value2",
    ..
  }


.. _OpName:

OpName
^^^^^^

.. parsed-literal::

   "INSERT" | "UPDATE" | "DELETE"

Webhook response structure
--------------------------

A ``2xx`` response status code is deemed to be a successful invocation of the webhook. Any other response status will be
deemed as an unsuccessful invocation which may cause retries as per the retry configuration.

It is also recommended that you return a JSON object in your webhook response.
