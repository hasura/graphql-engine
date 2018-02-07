.. .. meta::
   :description: Overview of the single query endpoint, /v1/query, exposed by Hasura's Data microservice and its Request and Response structure.
   :keywords: hasura, docs, data, query endpoint

API
===

``/v1/query``
-------------

The data microservice unifies all operations that can be performed on the database under
a single 'query' interface. A query is ``POST`` ed to ``/v1/query``.

A typical query operation is as follows:

Request
^^^^^^^

.. code-block:: http

     POST /v1/query HTTP/1.1

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

   * - ``"create_query_template"``
     - :ref:`create_query_template <create_query_template>`
     - Create a query template

   * - ``"execute_query_template"``
     - :ref:`execute_query_template <execute_query_template>`
     - Execute a query template

   * - ``"drop_query_template"``
     - :ref:`drop_query_template <drop_query_template>`
     - Drop an existing query template

   * - ``"set_query_template_comment"``
     - :ref:`set_query_template_comment <set_query_template_comment>`
     - Set comment on an existing query template

   * - ``"bulk"``
     - :ref:`Query <query_def>` array
     - Execute multiple operations in a single query

Response
^^^^^^^^

The response structure is dependent on the type of query that is executed.

``/v1/template``
----------------

This endpoint is used to execute an existing query template. You can read more about this :ref:`here <execute_query_template>`.

Errors
------

.. list-table::
   :widths: 10 10 30
   :header-rows: 1

   * - Status code
     - Description
     - Response structure

   * - ``200``
     - Success
     - .. parsed-literal::

          Request specific

   * - ``400``
     - Bad request
     - .. code-block:: haskell

          {
              "path"  : String,
              "error" : String
          }

   * - ``401``
     - Unauthorized
     - .. code-block:: haskell

          {
              "error" : String
          }

   * - ``500``
     - Internal server error
     - .. code-block:: haskell

          {
              "error" : String
          }

Error Codes
-----------

.. csv-table::
   :file: dataerrors.csv
   :widths: 10, 20, 70
   :header-rows: 1

