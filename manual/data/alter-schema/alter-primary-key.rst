Altering primary key of a table
===============================

You can modify the existing primary key using the :doc:`API console <../../api-console/index>` or via the API.

**Using Run SQL page**

Head to the ``Data > SQL`` section of API console and execute the following SQl query.

.. code-block:: sql

   ALTER TABLE category
      DROP CONSTRAINT category_pkey;
   ALTER TABLE category
      ADD PRIMARY KEY (name);

.. note::
   Check the *This is a migration* checkbox to retain the query as a migration.

First, you have to drop the existing primary key constraint before adding the new one. If you don't have a primary key
already, then skip the *DROP CONSTRAINT* statement in the above query.

**Using the API**

.. code-block:: http

      POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
      Authorization: Bearer <auth-token> # optional if cookie is set
      X-Hasura-Role: admin
      Content-Type: application/json

      {	
    		"type" : "run_sql",
    		"args" : {
    			"sql" : "ALTER TABLE category 
    				   DROP CONSTRAINT category_pkey;
    				   ALTER TABLE category 
    				   ADD PRIMARY KEY (name);"
    		}
      }

First, you have to drop the existing primary key constraint before adding the new one. If you don't have a primary key
already, then skip the *DROP CONSTRAINT* statement in the above API.

.. note::
      You cannot retain the query as a migration using the API