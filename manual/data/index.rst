.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Data
====

This section assumes that you have already created a project and know how to
launch your API console. If not, please head to :ref:`getting-started`.

The data APIs can be used to store and retrieve data. The actual data is stored
in Postgres tables. To fetch associated data, one can define relationships on
tables. Permissions can then be used to authorize the access to data based on
roles.

Tables
------

1. Using the console UI. Head to ``Data > Schema``.

   First launch the console:

   .. code-block:: bash

      $ hasura api-console

   This will open the API console. Head to ``Data > Schema``.

   .. image:: ../../img/manual/data/create-table.png


2. You can also create tables using SQL by heading to ``Data>SQL`` section in the console.

   .. image:: ../../img/manual/data/run_sql.png

3. or using the API:

   .. code-block:: http

      POST data.<project-name>.hasura-app.io/v1/query HTTP/1.1
      Authorization: Bearer <admin-token>
      Content-Type: application/json

      {
          "type" : "run_sql",
          "args" : {
              "sql" : "CREATE TABLE category (
                           id SERIAL NOT NULL PRIMARY KEY,
                           name TEXT NOT NULL
                       );"
          }
      }

.. toctree::
  :maxdepth: 1

  Select<select>
  Update<update>
  Insert<insert>
  Delete<delete>
  Permissions and access control<permissions>
  Linking data to users<linking_users_auth>
  Bulk<bulk>
  data_modeling/index
  Using SQL directly on data<using_sql>
  Data/schema migrations<data_migration/index>
  Aggregations <aggregations>
  Accessing postgres directly<access_postgres>

