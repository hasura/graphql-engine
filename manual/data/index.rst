.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Querying data (data APIs)
====================

This section assumes that you have already created a project and know how to launch your API console. If not, please head to
`hasura.io <https://hasura.io>`_  and get started!

The data APIs can be used to store and retrieve data. The actual data is stored in Postgres tables. To fetch associated data, one can define relationships on tables. Permissions can then be used to authorize the access to data based on roles.

.. Tables
.. --------
.. 
.. 1. Using the console UI. Head to ``Data > Schema``.
.. 
..    First launch the console:
.. 
..    .. code-block:: bash
.. 
..       $ hasuractl api-console
.. 
..    This will open the API console. Head to ``Data > Schema``.
.. 
..    .. image:: ../../img/manual/data/create-table.png
.. 
.. 
.. 2. You can also create tables using SQL by heading to ``Data>SQL`` section in the console.
.. 
..    .. image:: ../../img/manual/data/run_sql.png
.. 
.. 3. or using the API:
.. 
..    .. code-block:: http
.. 
..       POST data.<project-name>.hasura-app.io/v1/query HTTP/1.1
..       Authorization: Bearer <admin-token>
..       Content-Type: application/json
.. 
..       {
..           "type" : "run_sql",
..           "args" : {
..               "sql" : "CREATE TABLE category (
..                            id SERIAL NOT NULL PRIMARY KEY,
..                            name TEXT NOT NULL
..                        );"
..           }
..       }

.. toctree::
  :maxdepth: 1
  
  Select<select>
  Update<update>
  Insert<insert>
  Delete<delete>
  Bulk<bulk>
