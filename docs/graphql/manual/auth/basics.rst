Access control basics
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

In this section, we're going to set up a simple access control rule for restricting querying on a table.
We're working with a simple ``author`` table where users have some information stored about themselves.

Create a table
--------------

Head to your console and :ref:`create a table <create-tables>` called ``author`` with the following schema:

.. code-block:: sql

  author (
    id INT PRIMARY KEY,
    name TEXT
  )

Insert some sample data into the table:

+-------------+----------+
|      **id** | **name** |
+-------------+----------+
| 1           |  john    |
+-------------+----------+
| 2           |  shruti  |
+-------------+----------+
| 3           |  celine  |
+-------------+----------+
| 4           |  raj     |
+-------------+----------+

Try out a query
---------------

Head to the ``GraphiQL`` tab in your console and try out the below query:

.. code-block:: graphql

  query {
    author {
      id
      name
    }
  }

You'll see that this results in a response that contains all the authors because by default the GraphQL query is
accepted with **admin** permissions.

.. image:: ../../../img/graphql/manual/auth/fetch-authors.png


Add a simple access control rule for a logged in user
-----------------------------------------------------

Let's say that for our app, logged in users are only allowed to fetch their own data.

Head to the ``Permissions`` tab of the ``author`` table.

Let's add a **select** permission for the **user** role on the ``author`` table:

.. image:: ../../../img/graphql/manual/auth/author-select-perms.png

This reads as:

.. list-table::
   :header-rows: 1
   :widths: 15 20 25 40

   * - Table
     - Definition
     - Condition
     - Representation

   * - author
     - user's own row
     - ``id`` in the row is equal to ``user-id`` from the request session
     -
       .. code-block:: json

          {
            "id": {
              "_eq": "X-Hasura-User-Id"
            }
          }

Now, let's make the same query as above but also include two dynamic authorization variables ``X-Hasura-Role`` and
``X-Hasura-User-Id`` via request headers. These will automatically get used according to the permission rule we set up.

.. image:: ../../../img/graphql/manual/auth/query-with-perms.png

You can notice above how the same query now only includes the right slice of data.

.. admonition:: Permission rules can also use nested object's fields

  For example, for an ``article`` table with nested ``author`` table, we can define the select permission as:

  .. code-block:: json

    {
      "author" : {
        "id": {
          "_eq": "X-Hasura-User-Id"
        }
      }
    }

.. _restrict_columns:

Restrict access to certain columns
----------------------------------

We can restrict the columns of a table that a particular role has access to.

Head to the ``Permissions`` tab of the table and edit the ``Select`` permissions for the role:

.. image:: ../../../img/graphql/manual/auth/restrict-columns.png

.. _limit_rows:

Limit number of rows returned in a single request
-------------------------------------------------

We can set a hard limit on the maximum number of rows that will be returned in a single request for a table for a particular role.

Head to the ``Permissions`` tab of the table and edit the ``Select`` permissions for the role:

.. image:: ../../../img/graphql/manual/auth/limit-results.png

More about permissions
----------------------

Next: :doc:`Roles and dynamic variables <roles-variables>`


