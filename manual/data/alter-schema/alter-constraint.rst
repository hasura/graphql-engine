Altering Constraints
====================

You can add ``Unique`` constraints or modify the existing primary key by heading to the ``Data > SQL`` section of :doc:`API console <../../api-console/index>` or via the API.

**1. Using the API to alter ``unique`` constraint:**

.. code-block:: http

      POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
      Authorization: Bearer <auth-token> # optional if cookie is set
      X-Hasura-Role: <role>  # optional. Required if request needs particular user role
      Content-Type: application/json

      {	
    		"type" : "run_sql",
    		"args" : {
    			"sql" : "ALTER TABLE article 
    				 ADD CONSTRAINT unique_author UNIQUE (author_id);"
    		}
      }

**2. Using the API to alter ``primary key`` constraint:**

.. code-block:: http

      POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
      Authorization: Bearer <auth-token> # optional if cookie is set
      X-Hasura-Role: <role>  # optional. Required if request needs particular user role
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

First, you have to drop the existing primary key constraint before adding the new one. If you don't have a primary key already, then skip the *DROP CONSTRAINT* statement in the above API.

.. note::
      You cannot retain the query as a migration using the API