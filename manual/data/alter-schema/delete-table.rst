Deleting a Table
----------------

You can delete a ``table`` via the :doc:`API console <../../api-console/index>` or via the API.

**1. Using the console UI - Modify page**
	
You can remove a table by heading to ``Data > Schema > [table-name] > Modify`` section in the API console. Just click on ``Delete table`` button and confirm the removal.

.. note::
      When you delete a table, it is CASCADE disabled by default. It means that this table will not be deleted if other tables are dependent on it. In that case, use an API with CASCADE to delete dependent tables too.

**2. Using the API to delete a ``table``:**

.. code-block:: http

      POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
      Authorization: Bearer <admin-token>
      Content-Type: application/json

      {	
    		"type" : "run_sql",
    		"args" : {
    			"sql" : "DROP TABLE article"
    		}
      }

In the above API, ``article`` table will be deleted if there are no dependent objects. In case, article table is being depended by other tables, like author, the delete won't go through. In that case, just modify the SQL statement to ``DROP TABLE article CASCADE``.
