.. meta::
   :description: Manage multiple column + row permissions for the same role in Hasura
   :keywords: hasura, docs, authorization, access control, permission, role

.. _role_multiple_rules:

Multiple column + row permissions for the same role
===================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Use case
--------

In some cases we might want to allow access to certain columns for a role only if a particular condition is met
while allowing access to other columns based on a different condition
i.e. have different column permissions based on different row permissions.

Currently it is not possible to define multiple column + row permission rules for the same role.

We can work around this limitation by using :ref:`views <custom_views>`.

**Example**

Let's say we have a table called ``user_info`` with columns ``(id, name, city, email, phone, address)``.

We want the role ``user`` to be able to access:

- the ``email``, ``phone`` and ``address`` columns only if the ``id`` column is the requesting user's id i.e. the current
  user is the owner of the row.

- the ``id``, ``name`` and ``city`` columns for all rows.

We can achieve this via the following steps:

Step 1: Create a view 
---------------------

:ref:`Create a view <create_views>` called ``user_private`` with columns ``(user_id, email, phone, address)``:

.. code-block:: SQL

    CREATE VIEW user_private AS
      SELECT id AS user_id, email, phone, address
        FROM user_info;

Step 2: Create a relationship
-----------------------------

For the table ``user_info``, :ref:`create a manual object relationship <create_manual_relationships>` called
``private_info`` using ``user_info : id -> user_private : user_id``:

.. thumbnail:: ../../../../img/graphql/manual/auth/multiple-rules-create-manual-relationship.png
   :alt: Create a manual object relationship

Step 3: Define permissions
--------------------------

For the role ``user``, create the following permissions for ``select``:

- Table ``user_info``: allow access to ``id``, ``name`` and ``city`` without any row conditions.

.. thumbnail:: ../../../../img/graphql/manual/auth/multiple-rules-define-public-permissions.png
   :alt: Column access for the role user

- View ``user_private``: allow access to ``id``, ``phone``, ``email`` and ``address`` if the ``user-id``
  passed in the session variable is equal to the row's ``user_id``.

.. thumbnail:: ../../../../img/graphql/manual/auth/multiple-rules-define-private-permissions.png
   :alt: Column access for the role user based on row level permissions

Step 4: Query with appropriate access control
---------------------------------------------

Now we can fetch the required data with the appropriate access control by using the relationship.

If the ``X-Hasura-Role`` and the ``X-Hasura-User-Id`` session variables are set to ``user`` and ``2`` respectively, we'll get the following result:

.. graphiql::
  :view_only:
  :query:
    query {
      user_info {
        id
        name
        city
        private_info {
          email
          phone
          address
        }
      }
    }
  :response:
    {
      "data": {
        "user_info": [
          {
            "id": 1,
            "name": "Julie",
            "city": "Boston",
            "private_info": null
          },
          {
              "id": 2,
              "name": "Josh",
              "city": "Bangalore",
              "private_info": {
                "email": "josh@josh.com",
                "phone": "+91-9787675678",
                "address": "#141, 7th Main Road, Koramangala 3rd Block",
              }
            },
            {
              "id": 3,
              "name": "John",
              "city": "Berlin",
              "private_info": null
            }
        ]
      }
    }

Observe that the ``private_info`` field is returned as ``null`` for all rows without the appropriate access.
