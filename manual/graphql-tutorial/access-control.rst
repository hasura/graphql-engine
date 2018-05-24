Part V: Access Control
======================

Now that we have the schema ready, lets go and try out the following sample query that fetches all articles along with the comments (from the comment table we created) using the comments relationship:

.. code-block:: none

  query fetch_article {
      article {
        title
        comments {
          comment
        }
      }
  }

Go to the API-Explorer section in the API-Console and choose ``GraphQL`` in the left panel:

Type the query and press Play button:

.. image:: img/graphql_query_perm_fail.png

If you tried it, the query must have failed throwing an error like:

.. code-block:: json

    {
      "errors": [
        {
          "path": "$[0].args.columns[1]",
          "error": "role \"anonymous\" does not have permission to read relationship \"comments\"; no permission on table \"comment\"",
          "code": "permission-denied"
        }
      ]
    }

This is because we have not set the permissions for the ``comments`` table that we created.

By default, the data APIs can only be accessed by users with the ``admin`` role. However,
our app users will not have the admin role assigned. So, we need to allow access to the data APIs for
roles other than ``admin``. This is handled by the permission layer of the ``data`` microservice,
which lets you define row level and column level access control policies for all the roles for different query types.

In our app, what roles do we have other than ``admin``?

#. ``user`` for logged in users
#. ``anonymous`` for users who haven't logged in.

We need to define permissions on all the tables that we have created so far for ``user`` and ``anonymous`` roles.

Let's look at the access conditions we should set up for our tables:

.. rst-class:: api_tabs
.. tabs::

   .. tab:: article

      .. list-table::
         :header-rows: 1
         :widths: 20 20 25 35

         * - Role
           - Query type
           - Columns
           - Rows
         * - anonymous
           - select
           - all columns
           - all rows
         * - anonymous
           - insert
           - --
           - not allowed
         * - anonymous
           - update
           - none
           - not allowed
         * - anonymous
           - delete
           - --
           - not allowed
         * - user
           - select
           - all columns
           - all rows
         * - user
           - insert
           - --
           - if user is the author
         * - user
           - update
           - title, content
           - if user is the author
         * - user
           - delete
           - --
           - if user is the author

   .. tab:: author

      .. list-table::
         :header-rows: 1
         :widths: 20 20 25 35

         * - Role
           - Query type
           - Columns
           - Rows
         * - anonymous
           - select
           - all columns
           - all rows
         * - anonymous
           - insert
           - --
           - not allowed
         * - anonymous
           - update
           - none
           - not allowed
         * - anonymous
           - delete
           - --
           - not allowed
         * - user
           - select
           - all columns
           - all rows
         * - user
           - insert
           - --
           - their own row
         * - user
           - update
           - name
           - their own row
         * - user
           - delete
           - --
           - their own row

   .. tab:: like

      .. list-table::
         :header-rows: 1
         :widths: 20 20 25 35

         * - Role
           - Query type
           - Columns
           - Rows
         * - anonymous
           - select
           - all columns
           - all rows
         * - anonymous
           - insert
           - --
           - not allowed
         * - anonymous
           - update
           - none
           - not allowed
         * - anonymous
           - delete
           - --
           - not allowed
         * - user
           - select
           - all columns
           - all rows
         * - user
           - insert
           - --
           - if user is giving the like
         * - user
           - update
           - none
           - not allowed
         * - user
           - delete
           - --
           - if user gave the like

   .. tab:: comment

      .. list-table::
         :header-rows: 1
         :widths: 20 20 25 35

         * - Role
           - Query type
           - Columns
           - Rows
         * - anonymous
           - select
           - all columns
           - all rows
         * - anonymous
           - insert
           - --
           - not allowed
         * - anonymous
           - update
           - none
           - not allowed
         * - anonymous
           - delete
           - --
           - not allowed
         * - user
           - select
           - all columns
           - all rows
         * - user
           - insert
           - --
           - if user is writing the comment
         * - user
           - update
           - comment
           - if user wrote the comment
         * - user
           - delete
           - --
           - if user wrote the comment or is author of the article

To summarize:

* ``anonymous`` role users can select (read) all the data
* ``anonymous`` role users cannot modify (insert/delete/update) any data.
* ``user`` role users can select (read) all the data.
* ``user`` role users can insert/delete "their own data" and update only certain fields once inserted.

To define "their own data", we can describe a condition using the value of the ``X-Hasura-User-Id`` header passed to
the data microservice by the API gateway.

The following are the conditions we will use while setting up the row level permissions described above:

.. list-table::
   :header-rows: 1
   :widths: 15 20 25 40

   * - Table
     - Definition
     - Condition
     - Representation
   * - All tables
     - allow all rows
     - Without any checks
     -
       .. code-block:: json

          {}

   * - article
     - user is author
     - user-id is equal to ``author_id``
     -
       .. code-block:: json

          {
            "author_id": {
              "$eq": "X-Hasura-User-Id"
            }
          }

   * - author
     - user's own row
     - user-id is equal to ``id``
     -
       .. code-block:: json

          {
            "id": {
              "$eq": "X-Hasura-User-Id"
            }
          }

   * - like
     - user gave like
     - user-id is equal to ``user_id``
     -
       .. code-block:: json

          {
            "user_id": {
              "$eq": "X-Hasura-User-Id"
            }
          }

   * - comment
     - user wrote comment
     - user-id is equal to ``user_id``
     -
       .. code-block:: json

          {
            "user_id": {
              "$eq": "X-Hasura-User-Id"
            }
          }

   * - comment
     - user wrote comment or is author of article
     - user-id is equal to ``user_id`` or user-id is equal to ``article's author_id`` (this requires a relationship
       called ``article`` to be defined first)
     -
       .. code-block:: json

          {
            "$or": [
              {
                "user_id": {
                  "$eq": "X-Hasura-User-Id"
                }
              },
              {
                "article": {
                  "author_id": {
                    "$eq": "X-Hasura-User-Id"
                  }
                }
              }
            ]
          }

Defining permissions:
---------------------
We can use the ``API console`` UI to add permissions for our tables. Head to *Data -> [table-name] -> Permissions* to
see/modify the permissions on the table.

**For example**, let's set the ``update`` permissions for ``user`` role on the ``article`` table:

The *Permissions* tab of the ``article`` table should look like this:

.. image:: ../../img/complete-tutorial/tutorial-permissions-tab.png

Click on the *Edit* icon next to the user/update cell. It should open up an edit section like this:

.. image:: ../../img/complete-tutorial/tutorial-permissions-edit-empty.png

Now, set the permissions as described above. It should finally look like this:

.. image:: ../../img/complete-tutorial/tutorial-permissions-edit-filled.png

Hit *Save permissions* to save our changes.

Similary, set permissions for all the cases we have described above.

Once you have set the permissions, you can try the anonymous query to fetch the articles along with comments and it will work because we have set anonymous read permissions on all the tables:

.. image:: img/graphql_query_perm_success.png


Next: Customise Schema with Postgres Views
------------------------------------------

Next, let's head to :doc:`customise-schema-views`.
