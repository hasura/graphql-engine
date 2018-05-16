Altering schema of a table
==========================

You can add/delete columns to a table as well as alter the existing columns' type, default value and Nullable properties using
the following methods:

.. rst-class:: api_tabs
.. tabs::

   .. tab:: API-Console UI

      Head to ``Data > Schema > [table-name] > Modify`` section of :doc:`API console <../../api-console/index>`.

      Click on ``Edit`` for the column you want to alter/delete. A new column can be added via the Add column section at the bottom.

      .. image:: ../../../img/manual/data/alter-table.png

   .. tab:: API-Console SQL page

      You can also alter column using SQL by heading to ``Data > SQL`` section of :doc:`API console <../../api-console/index>`.

      .. image:: ../../../img/manual/data/alter-column-sql.png

      .. note::
            You should click on ``This is a migration`` option before executing the query if you want to retain the migration.

   .. tab:: REST API

      .. code-block:: http

            POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
            Authorization: Bearer <auth-token> # optional if cookie is set
            X-Hasura-Role: admin
            Content-Type: application/json

            {
                  "type" : "run_sql",
                  "args" : {
                      "sql" : "ALTER TABLE article
                           ALTER COLUMN rating TYPE numeric;"
                  }
            }

      .. note::
            You cannot retain the query as a migration using the API