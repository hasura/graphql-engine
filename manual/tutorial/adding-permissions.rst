Part IX: Permissions & access control on data models
====================================================

By default, the data APIs can only be accessed by users with the ``admin`` role. However,
our blog app users will not have the admin role assigned. So, we need to allow access to the data APIs for
roles other than ``admin``. This is handled by the permission layer of the ``data`` microservice,
which lets you define row level and column level access control policies for all the roles for different query types.

In our blog app, what roles do we have other than ``admin``?

#. ``user`` for logged in users
#. ``anonymous`` for users who haven't logged in.

We need to define permissions on all the tables that we have created so far (where applicable) for ``user`` and
``anonymous`` roles.

Defining permissions for different query types:
-----------------------------------------------

We can use the ``API console`` UI to add permissions.

Select
~~~~~~

Let us consider the ``article`` table. The following are the permissions we'd like to define for select queries on this table:

.. list-table::
   :header-rows: 1

   * - Role
     - Columns
     - Rows
   * - anonymous
     - all columns
     - all rows
   * - user
     - all columns
     - all rows

In the ``API console``, navigate to *Data -> article -> Permissions*.

This is the permissions section for the ``article`` table, which looks like this:

.. image:: ../../img/complete-tutorial/tutorial-9-vanilla-screen.png

To add permissions, click the *Edit icon to the corresponding role and query type*:

.. image:: ../../img/complete-tutorial/tutorial-9-add-permission.png
	    
You can add permissions for the query types Select, Insert, Update, Delete for different roles (default anonymous and user).
	    
Add permissions for the *Select* query for the *user* role.

.. image:: ../../img/complete-tutorial/tutorial-user-select-permission.png

Click *Save permissions* to apply the permissions.

You can use the same UI to add permissions for other query types.

Update
~~~~~~

``anonymous`` role cannot update the data in ``article``, in fact, any table. You don't need to configure anything for this as only ``admin`` role has permissions by default while the other permissions have to be configured.

.. list-table::
   :header-rows: 1

   * - Role
     - Columns
     - Rows
   * - anonymous
     - None
     - None
   * - user
     - title, content
     - those written by the user

.. image:: ../../img/complete-tutorial/tutorial-update-permission.png

Set the permissions similarly to the Select query.

Delete
~~~~~~

``anonymous`` role cannot delete the data in ``article`` table.

.. list-table::
   :header-rows: 1

   * - Role
     - Rows
   * - anonymous
     - None
   * - user
     - those written by the user

Set the permissions similarly to the Select query.

With delete, you only get to specify the rows that are allowed to be deleted with ``filter``.

.. image:: ../../img/complete-tutorial/tutorial-delete-permission.png

Insert
~~~~~~

``anonymous`` cannot insert into ``article`` table. If you are a user, you should only be able to create an article with you as the author, i.e, you should not be allowed to set arbitrary ``author_id`` when inserting into ``article`` table. This is an assertion that must be verified before the data is persisted.

.. image:: ../../img/complete-tutorial/tutorial-insert-permission.png

Set the permissions similarly to the Select query.

Permissions for all tables
~~~~~~~~~~~~~~~~~~~~~~~~~~

We've looked at the permissions on ``article`` table. Let's wrap this section by defining the permissions on all tables.

To define permissions on all tables you can follow the method above for each table. A better way to do it is to wrap all the queries into a **bulk** HTTP request:

.. code-block:: http

  POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
  Content-Type: application/json
  Authorization: Bearer <auth-token> # optional if cookie is set
  X-Hasura-Role: <role>  # optional. Required if request needs particular user role

    {
	"type": "bulk",
	"args": [
	    {
		"type": "create_insert_permission",
		"args": {
		    "table": "author",
		    "role": "user",
		    "permission": {
			"check": {
			    "id": "X-HASURA-USER-ID"
			}
		    }
		}
	    },
	    {
		"type": "create_select_permission",
		"args": {
		    "table": "author",
		    "role": "user",
		    "permission": {
			"columns": "*",
			"filter": {}
		    }
		}
	    },
	    {
		"type": "create_select_permission",
		"args": {
		    "table": "author",
		    "role": "anonymous",
		    "permission": {
			"columns": "*",
			"filter": {}
		    }
		}
	    },
	    {
		"type": "create_insert_permission",
		"args": {
		    "table": "comment",
		    "role": "user",
		    "permission": {
			"check": {
			    "author_id": "X-HASURA-USER-ID"
			}
		    }
		}
	    },
	    {
		"type": "create_select_permission",
		"args": {
		    "table": "comment",
		    "role": "user",
		    "permission": {
			"columns": "*",
			"filter": {}
		    }
		}
	    },
	    {
		"type": "create_update_permission",
		"args": {
		    "table": "comment",
		    "role": "user",
		    "permission": {
			"columns": [
			    "comment"
			],
			"filter": {
			    "author_id": "X-HASURA-USER-ID"
			}
		    }
		}
	    },
	    {
		"type": "create_select_permission",
		"args": {
		    "table": "comment",
		    "role": "anonymous",
		    "permission": {
			"columns": "*",
			"filter": {}
		    }
		}
	    }
	]
    }

Next: Add aggregations and views
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Next, head to :doc:`aggregations-views`.
