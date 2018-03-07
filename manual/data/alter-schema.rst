Altering database schema
========================

After creating tables, you would want to modify the table's columns, constraints, relationships and permissions. This section walks through common schema modification scenarios.

Alter Column
------------

You can alter a column's type, default value and Nullable property via multiple ways.

**1. Using the console UI - Modify page of a table**

   First launch the API console:

   .. code-block:: bash

      $ hasura api-console

   This will open the API console. Head to ``Data > Schema > article > Modify``.
   Click on Edit for the column you want to alter.

   .. image:: ../../img/manual/data/alter-column.png

**2. Using the console UI - SQL page**
	
   You can also alter column using SQL by heading to ``Data > SQL`` section in the console.

   .. image:: ../../img/manual/data/alter-column-sql.png

   Note: You should click on ``This is a migration`` option before executing the query if you want to retain the migration.

**3. Using the API:**

   .. code-block:: http

      POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
      Authorization: Bearer <admin-token>
      Content-Type: application/json

      {	
    		"type" : "run_sql",
    		"args" : {
    			"sql" : "ALTER TABLE article 
    				 ALTER COLUMN rating TYPE numeric;"
    		}
      }

Alter Constraints
-----------------

You can add ``Unique`` constraints or modify the existing primary key via SQL page of API console UI (just like the above example) or via the API.

**1. Using the API to alter ``unique`` constraint:**

   .. code-block:: http

      POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
      Authorization: Bearer <admin-token>
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
      Authorization: Bearer <admin-token>
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

First, you have to drop the existing primary key constraint before adding the new one. If you don't have a primary key already, then skip the DROP CONSTRAINT statement in the above API.

Deleting a Foreign Key
----------------------

You can delete a ``Foreign key`` constraint(s) via

**1. Using the console UI - modify page**

   You can remove foreign key constraint by heading to ``Data > Schema > article > Modify`` section in the console. Just click on remove constraint button and confirm the removal.

   .. image:: ../../img/manual/data/remove-constraint.png

**2. Using the API to delete a ``foreign key`` constraint:**

   .. code-block:: http

      POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
      Authorization: Bearer <admin-token>
      Content-Type: application/json

      {	
    		"type" : "run_sql",
    		"args" : {
    			"sql" : "ALTER TABLE article 
    					DROP CONSTRAINT article_category_id_fkey;"
    		}
      }

**3. SQL page of API console UI (like the first example above) or via the API:**

In this case, we are removing a foreign key constraint from ``article`` table for ``category_id`` column.

Deleting a Relationship
-----------------------

You can delete a ``relationship`` via Relationship page of API console UI (just like the first example above) or via the API.

**1. Using the console UI - Relationships page**
	
   You can remove a relationship by heading to ``Data > Schema > article > Relationship`` section in the console. Just click on remove button and confirm the removal.

   .. image:: ../../img/manual/data/remove-constraint.png

Note: You can also see Suggested relationships in the UI. That's the easiest way to quickly add a relationship. The suggested relationship is determined by the foreign key definition across the tables.

**2. Using the API to delete a ``relationship``:**


   .. code-block:: http

      POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
      Authorization: Bearer <admin-token>
      Content-Type: application/json

      {
      	"type" : "drop_relationship",
      	"args" : {
      		"table" : "article",
      		"relationship" : "author"
      	}
      }

In this case, we are removing a relationship from ``article`` table which was pointing to author table. (using author_id column). Once you have removed a relationship, it will come back to Suggested relationship section of the console UI.

Deleting a Permission
---------------------

You can delete a ``permission`` via Permission page of API console UI (just like the first example above) or via the API.

**1. Using the console UI - Permission page**
	
   You can remove a permission by heading to ``Data > Schema > article > Permission`` section in the console. Just click on remove button and confirm the removal.

   .. image:: ../../img/manual/data/delete-permissions.png

Note: You can also quick apply permissions in the UI. That's the easiest way to quickly add a permission. ``Public`` and ``Read only`` are the most common scenarios that can be quickly applied for a particular role.

**2. Using the API to delete a ``permission``:**

   .. code-block:: http

      POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
      Authorization: Bearer <admin-token>
      Content-Type: application/json

      {
      	"type" : "drop_select_permission",
      	"args" : {
      		"table" : "article",
      		"role"  : "anonymous"
      	}
      }

In this case, we are removing select permission from ``article`` table for the anonymous role. Similarly you can use the API to drop insert, update or delete permission if it was added.

Deleting a Table 
----------------

You can delete a ``table`` via Modify page of API console UI (just like the first example above) or via the API.

**1. Using the console UI - Modify page**
	
   You can remove a table by heading to ``Data > Schema > article > Modify`` section in the console. Just click on Delete table button and confirm the removal.

Note: When you delete a table, it is CASCADE disabled by default. It means that this table will not be deleted if other tables are dependent on it. In that case, use an API with CASCADE to delete dependent tables too.

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
