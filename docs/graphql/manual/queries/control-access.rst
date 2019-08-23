Restrict access to certain fields
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

If you want to restrict access to sensitive fields in a table, you can either use views to expose only the safe fields
or :ref:`restrict access via permissions <col-level-permissions>`.

The following section describes setting up a view for this purpose.

**For example**: Say we have a table ``user_profile (id, name, email, phone, address)``, to restrict users to
only have access to the ``id``, ``name`` & ``email`` fields of other users, we can:

Step 1: Create a view
---------------------
Open the Hasura console and head to the ``Data -> SQL`` tab.

Create a view with data from only the required (or public) columns:

.. code-block:: SQL

    CREATE VIEW user_public AS
      SELECT id, name, email
        FROM user_profile;

Step 2: Modify permissions
--------------------------
You will need to revoke permission (if already granted) from the source table and grant access to the newly created
view. So, in our example, we do the following:

#. Remove **select** permissions from the ``user_profile`` table

#. Grant **select** permissions to the ``user_public`` view

Step 3: Query the view
----------------------
You can now query the newly created view like you would a regular table:

.. graphiql::
  :view_only:
  :query:
    query {
      user_public {
        id
        name
        email
      }
    }
  :response:
    {
      "data": {
        "user_public": [
          {
            "id": 1,
            "name": "Justin",
            "email": "justin@xyz.com"
          },
          {
            "id": 2,
            "name": "Beltran",
            "email": "beltran@xyz.com"
          },
          {
            "id": 3,
            "name": "Sidney",
            "email": "sidney@xyz.com"
          },
          {
            "id": 4,
            "name": "Angela",
            "email": "angela@xyz.com"
          }
        ]
      }
    }
