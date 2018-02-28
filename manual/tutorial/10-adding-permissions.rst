.. .. meta::
   :description: Part 6 of a set of learning exercises meant for exploring Hasura in detail. This part introduces access control for data when using the data API.
   :keywords: hasura, getting started, step 6, access control, role based access control

====================================================
Part X: Permissions & access control on data models
====================================================

We've mentioned that by default, the ``data`` APIs can only be accessed by users with the ``admin`` role. However, we should never include the admin token in any client applications. So, we need to allow access to the ``data`` APIs for roles other than ``admin``. This is handled by the permission layer of the ``data`` microservice, which lets you define row level and column level access control policies for various roles.

In our blog app, what roles do we have other than ``admin``?

#. ``user`` for logged in users
#. ``anonymous`` for users who haven't logged in.

We need to define permissions on all the tables that we have created so far (where applicable) for ``user`` and ``anonymous`` roles. As you've probably guessed, we can use both the console UI and the data API to create permissions.
Watch this video to see how permissions are defined using the console or continue reading to use the ``data`` API

.. admonition:: Video reference

   For a highly simplified version of this page and a super fast introduction
   check `this video <https://www.youtube.com/watch?v=lW7iz3cFqAg>`_ out!

Defining permissions
====================

Select
------

Let us consider the ``article`` table. The following are the permissions we'd like to define for select queries on this table:

.. list-table::
   :header-rows: 1

   * - Role
     - Columns
     - Rows
   * - anonymous
     - all columns
     - which are published
   * - user
     - all columns
     - which are published and those written by the user (published or not)
       
There are 2 methods to define permissions for a table: via the console UI and via a REST query.

Using the UI
^^^^^^^^^^^^
In the api-console, navigate to *Data -> article -> Permissions*.

This is the permissions section for the ``article`` table, which looks like this:

.. figure:: ../../img/tutorial-9-vanilla-screen.png

To add permissions, click the *Edit icon to the corresponding role and query type*:

.. figure:: ../../img/tutorial-9-add-permission.png
	    
	    You can add permissions for the query types Select, Insert, Update, Delete for different roles (default anonymous and user).
	    
Add permissions for the *Select* query for the *user* role.

.. figure:: ../../img/tutorial-9-permissions-user-select.png

Click *Save permissions* to apply the permissions.

You can use the same UI to add permissions for other query types. 

Using a REST Query
^^^^^^^^^^^^^^^^^^
You can also make a REST query to the *data* microservice to accomplish the same task as the above mentioned UI and more.

For **Select**, the HTTP query equivalent to the above UI-based flow looks like:

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "create_select_permission",
       "args" : {
	   "table" : "article",
	   "role" : "anonymous",
	   "permission" : {
		"columns" : "*",
		"filter" : {
		    "is_published" : true
		}
	   }
       }
   }

We've specified ``*`` for ``columns`` as a short hand notation for all columns. ``filter`` is used to specify the rows that can be read. You may have noticed that the syntax is similar to that of ``where`` in ``select`` query. In fact, they are exactly the same. The only difference is that, in ``filter`` you can use a special placeholder variable ``REQ_USER_ID`` for the ``id`` of the user who makes the query. We'll see this in action when defining the select permission for ``user`` role on ``article`` table.

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "create_select_permission",
       "args" : {
           "table" : "article",
           "role" : "user",
	   "permission" : {
		"columns" : "*",
		"filter" : {
		   "$or" : [
		      { "is_published" : true },
                      { "author_id" : "REQ_USER_ID" }
                    ]
                }
	   }
       }
   }

As discussed in Part VI, we know that the gateway forwards ``X-Hasura-*`` headers with each request. So, when a ``select`` query on ``article`` is made with a token representing some user with the role ``user``, the ``REQ_USER_ID`` is substituted with the ``X-Hasura-User-Id`` value and then the ``filter`` condition is applied.

Update
------

``anonymous`` role cannot update the data in ``article``, in fact, any table.

.. list-table::
   :header-rows: 1

   * - Role
     - Columns
     - Rows
   * - anonymous
     - None
     - None
   * - user
     - title, content, is_published
     - those written by the user

To set the permissions, you can use the api-console UI based workflow described above or the following REST call:

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "create_update_permission",
       "args" : {
	   "table" : "article",
	   "role" : "user",
	   "permission" : {
		"columns" : ["title", "content", "is_published"],
		"filter" : {
		   "author_id" : "REQ_USER_ID"
		}
	   }
       }
   }

Update permission syntax is the same as select permission's. You specify the columns that can be updated with ``columns`` and the rows that can be updated using ``filter``.

Delete
------

``anonymous`` role cannot delete the data in ``article`` table.

.. list-table::
   :header-rows: 1

   * - Role
     - Rows
   * - anonymous
     - None
   * - user
     - those written by the user

To set the permissions, you can use the api-console UI based workflow described above or the following REST call:

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "create_delete_permission",
       "args" : {
	   "table" : "article",
	   "role" : "user",
	   "permission" : {
		"filter" : {
		   "author_id" : "REQ_USER_ID"
		}
	   }
       }
   }

With delete, you only get to specify the rows that are allowed to be deleted with ``filter``.

Insert
------

``anonymous`` cannot insert into ``article`` table. If you are a user, you should only be able to create an article with you as the author, i.e, you should not be allowed to set arbitrary ``author_id`` when inserting into ``article`` table. This is an assertion that must be verified before the data is persisted.

To set the permissions, you can use the api-console UI based workflow described above or the following REST call:

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "create_insert_permission",
       "args" : {
	   "table" : "article",
	   "role" : "user",
	   "permission" : {
		"check" : {
		    "author_id" : "REQ_USER_ID"
		}
	   }
       }
   }

With insert, you only get to specify the assertion that has to be validated with ``check``.

Permissions for all tables
--------------------------

We've looked at the permissions on ``article`` table. Let's wrap this section by defining the permissions on all tables.

To define permissions on all tables you can follow the method above for each table. A better way to do it is to wrap all the queries into a **bulk** HTTP request:

.. code-block:: http

  POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
  Content-Type: application/json
  Authorization: <admin-token>

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
			    "hasura_id": "REQ_USER_ID"
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
			    "author_id": "REQ_USER_ID"
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
			    "author_id": "REQ_USER_ID"
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
	    },
	    {
		"type": "create_select_permission",
		"args": {
		    "table": "category",
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
		    "table": "category",
		    "role": "anonymous",
		    "permission": {
			"columns": "*",
			"filter": {}
		    }
		}
	    },
	    {
		"type": "create_select_permission",
		"args": {
		    "table": "article_category",
		    "role": "anonymous",
		    "permission": {
			"columns": "*",
			"filter": {
			    "article": {
				"is_published": true
			    }
			}
		    }
		}
	    },
	    {
		"type": "create_select_permission",
		"args": {
		    "table": "article_category",
		    "role": "user",
		    "permission": {
			"columns": "*",
			"filter": {
			    "article": {
				"$or": [
				    {
					"is_published": true
				    },
				    {
					"author_id": "REQ_USER_ID"
				    }
				]
			    }
			}
		    }
		}
	    },
	    {
		"type": "create_delete_permission",
		"args": {
		    "table": "article_category",
		    "role": "user",
		    "permission": {
			"filter": {
			    "article": {
				"author_id": "REQ_USER_ID"
			    }
			}
		    }
		}
	    },
	    {
		"type": "create_insert_permission",
		"args": {
		    "table": "article_category",
		    "role": "user",
		    "permission": {
			"check": {
			    "article": {
				"author_id": "REQ_USER_ID"
			    }
			}
		    }
		}
	    }
	]
    }
    
Next: Add aggregations and views
--------------------------------

Next, head to :doc:`Add aggregations & views to your data models <11-aggregations-views>`.
