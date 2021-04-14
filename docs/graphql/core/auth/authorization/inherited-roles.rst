.. meta::
   :description: Hasura inherited roles
   :keywords: hasura, docs, authorization, multiple roles, inherited roles

.. _inherited_roles:

Inherited roles
===============

.. contents:: Table of contents
   :backlinks: none
   :depth: 2
   :local:

Introduction
------------

An inherited role is a way to create a new role which infers permissions from two or more non-inherited roles. Once an inherited role is created, it can be treated as any other role i.e. can be given in ``X-Hasura-Role`` session variable.

Inherited roles are useful when you need to define multiple permission rules (may be overlapping) on schema objects and also for greater modularity in role management.

.. note::

   This feature is currently accessible as an experimental feature and must be enabled.
   This can be done either by setting the env var ``HASURA_GRAPHQL_EXPERIMENTAL_FEATURES``
   to ``inherited_roles`` or by providing the server flag ``--experimental-features``
   to ``inherited_roles``.

   See :ref:`server config reference <server_flag_reference>` for info on setting the flag/env var.

.. admonition:: Supported from

   Inherited roles are supported for versions ``v2.0.0-alpha.4`` and above.

Creating inherited roles
------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     Go to the ``Settings`` tab (⚙) on the console and click on ``Inherited Roles``.

     .. thumbnail:: /img/graphql/core/auth/console-inherited-role.png
        :alt: Console create inherited role
        :width: 1100px

  .. tab:: CLI

     To add a new inherited role, edit the ``metadata/inherited_roles.yaml`` file adding the inherited role definition
     like this:

     .. code-block:: yaml

        - role_name: sample_inherited_role
          role_set:
            - user
            - editor

     Apply the metadata by running:

     .. code-block:: bash

       hasura metadata apply

  .. tab:: API

     You can add a inherited role using the :ref:`add_inherited_role metadata API <metadata_add_inherited_role>`:

     .. code-block:: http

      POST /v1/metadata HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "add_inherited_role",
        "args": {
           "role_name":"sample_inherited_role",
           "role_set":[
              "user",
              "editor"
           ]
        }
      }


How is the permission of the inherited role inferred?
-----------------------------------------------------

A select permission is comprised of the following things:

1. Columns accessible to the role
2. Row selection filter
3. Limit
4. Allow aggregation
5. Scalar computed fields accessible to the role

.. note::

   Inherited roles can only combine SELECT permissions currently

Suppose there are two roles, ``role1`` gives access to column ``C1`` with row filter ``P1`` and ``role2`` gives access to columns ``C1`` and ``C2`` with row filter ``P2``. Consider the following GraphQL query executed with an inherited role comprised of ``role1`` and ``role2``:

.. code-block:: graphql

   query {
     T {
       C1
       C2
     }
   }

The above GraphQL query will be translated to the following SQL query.

.. code-block:: sql

    select (case when (P1 or P2) then C1 else null end) as C1,
           (case when P2 then C2 else null end) as C2
    from T
    where (P1 or P2)


The other parameters of the select permission will be combined in the following manner:

1. Limit - Minimum of the limits will be the limit of the inherited role
2. Allow aggregations - If any of the role allows aggregation, then the inherited role will allow aggregation
3. Scalar computed fields - same as table column fields, as in the above example

Accessibility of a field for an inherited role
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Accessibility of a field for an inherited role is defined as follows:

1. When all the roles give access to a column ``C``, then ``C`` will
   always be accessible.
2. When not all, but some of the roles give access to the column ``C``
   then the value of the column ``C`` will be outputed when the OR
   of ``P1,P2....P(n)`` is ``true`` and when it evaluates to ``false``,
   the value of the column ``C`` will be ``null``, where ``P`` is the
   row filter of the select permissions in which column ``C`` is given access to.
3. When none of the roles give access to column ``C``, it won't be accessible
   to the inherited role.

Examples
--------

Let's take the example of an ``users`` table with the following columns:

1. ``id`` - Int - Primary key
2. ``name`` - Text
3. ``email`` - Text

There are two roles defined namely ``employee`` and ``manager``.

1. User role - The user role will be able to able to access all columns of their row  when the session variable ``X-Hasura-User-Id`` is equal to the ``id``.

2. Anonymous role - The anonymous role will be able to access only the ``id`` and ``name`` columns of all the users.

Let's create a new inherited role called ``user_anonymous_inherited_role`` which inherits from the ``user`` and the ``anonymous`` roles.

1. Executing the query as ``user`` role

.. code-block:: http

   POST /v1/graphql HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: user
   X-Hasura-User-Id: 1

.. graphiql::
  :view_only:
  :query:
     query {
        users {
          id
          name
          email
        }
      }
  :response:
     {
       "data": {
         "users": [
           {
              "id": 1,
              "name": "alice",
              "email": "alice@xyz.com"
           }
         ]
       }
     }

2. Executing the query as ``anonymous`` role

.. code-block:: http

   POST /v1/graphql HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: anonymous

.. graphiql::
  :view_only:
  :query:
     query {
        users {
          id
          name
        }
      }
  :response:
     {
       "data": {
         "users": [
           {
             "id": 1,
             "name": "Alice"
           },
           {
             "id": 2,
             "name": "Bob"
           },
           {
             "id": 3,
             "name": "Sam"
           }
         ]
       }
     }

3. Executing the query as ``user_anonymous_inherited_role`` role

.. code-block:: http

   POST /v1/graphql HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: user_anonymous_inherited_role
   X-Hasura-User-Id: 1

.. graphiql::
   :view_only:
   :query:
      query {
        users {
          id
          name
          email
        }
      }
   :response:
      {
        "data": {
          "users": [
            {
              "id": 1,
              "name": "Alice",
              "email": "alice@xyz.com"
            },
            {
              "id": 2,
              "name": "Bob",
              "email": null
            },
            {
              "id": 3,
              "name": "Sam",
              "email": null
            }
          ]
        }
      }

In the response of the query being executed with the ``user_anonymous_inherited_role`` role, there are 3 rows returned and if
we compare that to the queries executed as the ``user`` and ``anonymous`` roles, the results are unioned in the inherited
role. But some of the fields have ``null`` values despite the value in the database not being ``null``. This can only happen
with inherited roles when a column doesn't have permission in the particular row. In the above example, we see that the
``email`` of "Bob"  and "Sam" is ``null`` but a non null value for "Alice", this is because the "Alice" row is executed as the
``user`` role and the other rows are executed as the ``anonymous`` role which is why is why the value is ``null``.


4. Suppose we have two tables ``users`` and ``authors`` and similarly two roles ``user`` and ``author`` are defined. The ``user``
   role doesn't have permission to query the ``authors`` table and the ``user`` role doesn't have permission to query the ``authors`` table. With only the ``user`` and the ``author`` role, we won't be able to construct a query which fetches data from both the tables. This can be solved by creating an inherited role out of ``user`` and ``author`` which can query both the
   tables in a single query.


.. code-block:: http

   POST /v1/graphql HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: user_authors_inherited_role
   X-Hasura-User-Id: 1

.. graphiql::
  :view_only:
  :query:
       query {
         users {
           id
           name
           email
         }
         authors {
           id
           name
           followers
         }
       }
  :response:
       {
         "data": {
           "users": [
             {
               "id": 1,
               "name": "Alice",
               "email": "alice@xyz.com"
             }
           ],
           "authors": [
             {
               "id": 1,
               "name": "Paulo Coelho",
               "followers": 10382193
             }
           ]
         }
       }


Present limitations
-------------------

Currently, inherited roles are supported only for Postgres read queries and subscriptions.
The following features are **not** supported for inherited roles yet:

1. Mutations
2. Actions
3. Remote schemas
