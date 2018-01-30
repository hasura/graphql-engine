.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _data-create-tables:

Create tables
=============

1. Using the console UI.

   First launch the API console:

   .. code-block:: bash

      $ hasura api-console

   This will open the API console. Head to ``Data > Schema``.

   .. image:: ../../img/manual/data/create-table.png


2. You can also create tables using SQL by heading to ``Data > SQL`` section in the console.

   .. image:: ../../img/manual/data/run_sql.png

   Note: You should click on ``This is a migration`` option before executing the query if you want to retain the migration.

3. or using the API:

   .. code-block:: http

      POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
      Authorization: Bearer <admin-token>
      Content-Type: application/json

      {
          "type" : "run_sql",
          "args" : {
              "sql" : "CREATE TABLE article (
                           id SERIAL NOT NULL PRIMARY KEY,
                           title TEXT NOT NULL,
                           description TEXT
                       );"
          }
      }

