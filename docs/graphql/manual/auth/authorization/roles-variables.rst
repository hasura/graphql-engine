Roles & Session variables
=========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Roles
-----
Every table/view can have permission rules for users based on their role.

By default, there is an ``admin`` role that can perform any operation on any table.

You can define roles and then create permissions for each of these roles:

Examples:

+-----------+-----------------------------------+---------------------------------------+
| user      | A logged-in user                  | CRUD on data that belongs to them     |
+-----------+-----------------------------------+---------------------------------------+
| anonymous | A not logged-in user              | Only read from some tables/views      |
+-----------+-----------------------------------+---------------------------------------+
| manager   | A user that  has access to other  | CRUD on all users data                |
|           | user's data                       |                                       |
+-----------+-----------------------------------+---------------------------------------+

.. admonition:: Role-based schemas

  For every role that you create, Hasura automatically publishes a different GraphQL schema that represents the
  right queries, fields, and mutations that are available to that role.


Dynamic session variables
-------------------------

When you create a permission, or an access control rule, the permission rule itself needs access to some variables
that are derived from the request itself. Let's refer to these as *session variables*.

For example: If a user makes a request, the session token maps to a ``user-id``. This ``user-id`` can be used in
a permission to show that inserts into a table are only allowed if the ``user_id`` column has a value equal to that
of ``user-id``, the session variable.

When you are constructing permission rules, however, there might be several variables that represent the business logic
of having access to data. For example, if you have a SaaS application, you might restrict access based on a ``client_id``
variable. If you want to provide different levels of access on different devices you might restrict access based on a
``device_type`` variable.

Hasura allows you to create permission rules that can use any dynamic variable that is a property of the request.
All your dynamic variables must follow the naming convention ``X-Hasura-*``.

Examples:

.. list-table::
   :header-rows: 1
   :widths: 20 10 20 50

   * - Example
     - Role
     - Condition
     - Permission expression

   * - Allow access to user's own row
     - ``user``
     - ``user_id`` column is equal to ``session-user-id`` from a request
     -
       .. code-block:: json

          {
            "user_id": {
              "_eq": "X-Hasura-User-Id"
            }
          }

   * - Allow project admins access to anything that belongs to the project
     - ``project-admin``
     - ``project_id`` column is equal to ``project-id`` of the "session user"
     -
       .. code-block:: json

          {
            "project_id": {
              "_eq": "X-Hasura-Project-Id"
            }
          }

Modelling Roles in Hasura
-------------------------

General guidelines for modelling roles in Hasura.

Roles are typically be modelled in two ways:

1. **Hierarchical roles**: Access scopes are nested depending on available roles. `Roles in Github for organisations <https://help.github.com/en/articles/managing-peoples-access-to-your-organization-with-roles>`_
   is a great example of such modelling where access scopes are inherited by deeper roles:

   .. thumbnail:: ../../../../img/graphql/manual/auth/github-org-hierarchical-roles.png

2. **Flat roles**: Non-hierarchical roles with each role requiring an independent access scope to be defined.

**Roles in Hasura have to be defined in the latter way i.e. in a flat, non-hierarchical model**.

To convert the above hierarchical roles model into the one expected by Hasura, you will need to model roles as
partially captured by the table below (*showing access permissions for the* ``user`` *&* ``org-member`` *roles*,
``repositories`` *table and* ``select`` *operation*):

.. list-table::
  :header-rows: 1
  :widths: 25 20 45

  * - Role
    - Access Permissions
    - Example permission rule

  * - user
    - Allow access to personally created repositories
    -
       .. code-block:: json

          {
            "creator_id": {
              "_eq": "X-Hasura-User-Id"
             }
           }

  * - org-member
    - Allow access to personally created repositories and the organisation's repositories.
    -
      .. code-block:: json

        {
          "_or": [
            {
              "creator_id": {
                "_eq": "X-Hasura-User-Id"
              }
            },
            {
              "organization": {
                "members": {
                  "member_id" : {
                    "_eq" : "X-Hasura-User-Id"
                  }
                }
              }
            }
          ]
        }

Making role-based user information available
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Effective permission rules require that information about which roles have access to which objects is available
when processing the permission rule. Different users with the same role or the same user with different roles
may have access to different sets of rows of the same table.

In some cases this is straightforward - for example, to restrict access for authors to only their articles, a
trivial row-level permission like ``"creator_id": {"_eq": "X-Hasura-User-Id"}`` will suffice. In others, like
our example in the previous section, this user information (*ownership or relationship*) must be available for
defining a permission rule.

These non-trivial use-cases are to handled differently based on whether this information is available in the same
database or not.

Relationship information is available in the same database
**********************************************************

Let's take a closer look at the permission rule for the ``org-member`` rule in the example from the previous
section. The rule reads as "*allow access to this repository if it was created by this user or if this user is
a member of the organisation that this repository belongs to*".

The crucial piece of user information, that is presumed to be available in the same database, that makes this an
effective rule is the mapping of users (*members*) to organizations.

Since this information is available in the same database, it can be easily leveraged via
:ref:`Relationships in permissions <relationships-in-permissions>` (*see this reference for another
example of the same kind*).

Relationship information is **not** available in the same database
******************************************************************

When this user information is not available in the database that Hasura is configured to use, session variables
are the only avenue to pass this information to a permission rule. In our example, the mapping of users (members)
to organizations may not have been in available in the same database.

To convey this information, a session variable, say ``X-Hasura-Allowed-Organisations`` can be used by the
configured authentication to relay this information. We can then check for the following condition to emulate
the same rule - *is the organization that this repository belongs to part of the list of the organizations the
user is a member of*.

The permission for ``org-member`` role changes to this:

.. code-block:: json

  {
    "_or": [
      {
        "creator_id": {
          "_eq": "X-Hasura-User-Id"
        }
      },
      {
        "organization_id": {
          "_in": "X-Hasura-Allowed-Organisations"
        }
      }
    ]
  }

.. admonition:: Arrays in permission rules

   Support for using session variables for array operators like ``_in``, ``_nin``, ``_has_any_keys``,
   ``_has_all_keys`` is only added in ``beta.3`` release.

Format of session variables
---------------------------

Session variables are currently expected to be Strings and should be encoded as Postgres's literals for
the relevant type.

For example, in the above example, let's say ``creator_id`` and ``organisation_id`` columns are of
type ``integer``, then the values of ``X-Hasura-User-Id`` and  ``X-Hasura-Allowed-Organisations`` should
be of type ``integer`` and ``integer[]`` (an integer array) respectively. To pass say a value ``1`` for
``X-Hasura-User-Id``, it'll be "``1``" and if the allowed organisations are ``1``, ``2`` and ``3``, then
``X-Hasura-Allowed-Organisations`` will be "``{1,2,3}``". ``{}`` is the syntax for specifying
`arrays in Postgres <https://www.postgresql.org/docs/current/arrays.html#ARRAYS-INPUT>`_.

The types and their formats are detailed `here <https://www.postgresql.org/docs/current/datatype.html>`_. When
in doubt about the Postgres format for a type, you can always test it in the SQL window. To check
if ``s`` is a valid literal for type ``t`` then, you can check it as follows:

.. code-block:: sql

   select 's'::t;

If the above command returns data, then ``s`` is a valid literal of type ``t``. For example, to check
if ``{hello,world}`` is a valid format of type ``text[]``, you can run:

.. code-block:: sql

   select '{hello,world}'::text[];

.. admonition:: JSON format

   In future, we'll add support for passing session variables as JSON values where possible (i.e, auth
   webhook and JWT but not in headers).
