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

Now, insert some sample data into the table using the ``Insert Row`` tab of the ``author`` table.

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

Let's say that we want to restrict users to fetch only their own data.

Head to the ``Permissions`` tab of the ``author`` table.

Now add a ``select`` access control rule for the ``user`` role on the ``author`` table:

.. image:: ../../../img/graphql/manual/auth/author-select-perms.png

This rule reads as:

.. list-table::
   :header-rows: 1
   :widths: 25 20 45

   * - Definition
     - Condition
     - Representation

   * - allow user to access only their own row
     - ``id`` in the row is equal to ``user-id`` from the request session variable
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

.. admonition:: Defining access control rules

  Access control, or permission rules can be as complex as you need them to be, even using a nested object's
  fields if required. You can use the same operators that you use to filter query results to define
  permission rules. See :doc:`filtering query results <../queries/query-filters>` for more details.

  For example, for an ``article`` table with a nested ``author`` table, we can define the select permission as:

  .. code-block:: json

    {
      "_and":
        [
          {
            "published_on": { "_gt": "31-12-2018" }
          },
          {
            "author": {
              "id": { "_eq": "X-Hasura-User-Id" }
            }
          }
        ]
      }

  This rule reads as: allow selecting an article if it was published after "31-12-2018" and its author is the current user.

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


