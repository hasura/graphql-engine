Event trigger payload spec
==========================
The payload that is submitted by the Hasura event system to configured webhooks has the following structure:

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

1. INSERT - the ``event.data.old`` will be ``null``. ``event.data.new`` will contain the insert row.
2. UPDATE - the ``event.data.old`` will be values before the update. ``event.data.new`` will contain the values after the update.
3. DELETE - the ``event.data.old`` will contain the row that is deleted. ``event.data.new`` will be ``null``.

**E.g. QUERY**:

.. code-block:: json

    {
      "event": {
          "op": "INSERT",
          "data": {
            "old": null,
            "new": {
                "id":"42",
                "name": "john doe"
            }
          }
      },
      "created_at": "2018-09-05T07:14:21.601701Z",
      "id": "85558393-c75d-4d2f-9c15-e80591b83894",
      "trigger": {
          "name": "test_trigger",
          "id": "37b7f91a-b3a5-4b85-be59-e5920d72f6aa"
      },
      "table": {
          "schema": "public",
          "name": "users"
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
