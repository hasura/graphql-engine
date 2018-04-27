.. .. meta::
   :description: Manual for using sql directly in the data query
   :keywords: hasura, docs, sql

.. _run_sql:

Running SQL commands on database
================================

If you want to run SQL statements directly on the database, this can be done in the following ways:

.. note::
   Currently, renaming of tables and columns are not allowed using SQL statements.


.. rst-class:: api_tabs
.. tabs::

   .. tab:: API-Console

      First launch the API console:

      .. code-block:: bash

         $ hasura api-console

      Head to ``Data > SQL`` section in the console.

      .. image:: ../../img/manual/data/alter-column-sql.png

      .. note::
         Check the ``This is a migration`` option before executing the query if you want to retain the query as a db migration.

         Check the ``Track table`` option before executing a query to create a table/view if you want the Data microservice
         to track it and enable access to it via the APIs.


   .. tab:: REST API

      The ``run_sql`` endpoint of the Data microservice can be used to run SQL
      statements directly on the data.

      ``run_sql`` is used to run arbitrary SQL statements. Multiple SQL statements can be separated by a ``;``, however, only the result of the last sql statement will be returned.

      An example:

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: admin

         {
             "type": "run_sql",
             "args": {
                 "sql": "CREATE UNIQUE INDEX ON films (title);"
             }
         }

      .. note::
         You cannot save the query as a migration using the API

         See the :doc:`API reference <../api-reference/data/query/misc>` for other
         caveats and details.

