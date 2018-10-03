JSON API endpoints
==================


/v1/query
---------

A typical query operation is as follows:

Request
^^^^^^^

.. code-block:: http

   POST /v1/query HTTP/1.1

   {
      "query": "<query-type>",
      "args": <args-object>
   }

Takes the JSON structure of a :ref:`Query <query_def>` for its body

.. _query_def:

``Query``
&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - type
     - true
     - String
     - Type of the query
   * - args
     - true
     - JSON Value
     - The arguments to the query

The various values that ``type`` and ``args`` can take are listed in the following table:

.. list-table::
   :header-rows: 1

   * - ``type``
     - ``args``
     - ``Synopsis``

   * - ``"create_object_relationship"``
     - :ref:`create_object_relationship <create_object_relationship>`
     - Define a new object relationship

   * - ``"create_array_relationship"``
     - :ref:`create_array_relationship <create_array_relationship>`
     - Define a new array relationship

   * - ``"drop_relationship"``
     - :ref:`drop_relationship <drop_relationship>`
     - Drop an existing relationship

   * - ``"set_relationship_comment"``
     - :ref:`set_relationship_comment <set_relationship_comment>`
     - Set comment on an existing relationship

   * - ``"create_insert_permission"``
     - :ref:`create_insert_permission <create_insert_permission>`
     - Specify insert permission

   * - ``"drop_insert_permission"``
     - :ref:`drop_insert_permission <drop_insert_permission>`
     - Remove existing insert permission

   * - ``"create_select_permission"``
     - :ref:`create_select_permission <create_select_permission>`
     - Specify select permission

   * - ``"drop_select_permission"``
     - :ref:`drop_select_permission <drop_select_permission>`
     - Remove existing select permission

   * - ``"create_update_permission"``
     - :ref:`create_update_permission <create_update_permission>`
     - Specify update permission

   * - ``"drop_update_permission"``
     - :ref:`drop_update_permission <drop_update_permission>`
     - Remove existing update permission

   * - ``"create_delete_permission"``
     - :ref:`create_delete_permission <create_delete_permission>`
     - Specify delete permission

   * - ``"drop_delete_permission"``
     - :ref:`drop_delete_permission <drop_delete_permission>`
     - Remove existing delete permission

   * - ``"set_permission_comment"``
     - :ref:`set_permission_comment <set_permission_comment>`
     - Set comment on an existing permission

   * - ``"bulk"``
     - :ref:`Query <query_def>` array
     - Execute multiple operations in a single query

   * - ``"insert"``
     - :ref:`insert <data_insert>`
     - Insert data into tables

   * - ``"select"``
     - :ref:`select <data_select>`
     - Retrieve data from tables

   * - ``"update"``
     - :ref:`update <data_update>`
     - Update data

   * - ``"delete"``
     - :ref:`delete <data_delete>`
     - Delete data

   * - ``"count"``
     - :ref:`count <data_count>`
     - Count the number of rows in a table

Response
^^^^^^^^

The response structure is dependent on the type of query that is executed.
