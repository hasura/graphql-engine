Restrict access to certain fields
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Expose a subset of columns
--------------------------

If you want to restrict access to sensitive fields in a table, you can either :ref:`restrict access via permissions <col-level-permissions>` or use views to expose only the safe fields.

The following section describes setting up a view for this purpose.

**For example**: Say we have a table ``user_profile (id, name, email, phone, address)``, to restrict users to
only have access to the ``id``, ``name`` & ``email`` fields of other users, we can:

Step 1: Create a view
^^^^^^^^^^^^^^^^^^^^^
Open the Hasura console and head to the ``Data -> SQL`` tab.

Create a view with data from only the required (or public) columns:

.. code-block:: SQL

    CREATE VIEW user_public AS
      SELECT id, name, email
        FROM user_profile;

Step 2: Modify permissions
^^^^^^^^^^^^^^^^^^^^^^^^^^
You will need to revoke permission (if already granted) from the source table and grant access to the newly created
view. So, in our example, we do the following:

#. Remove **select** permissions from the ``user_profile`` table

#. Grant **select** permissions to the ``user_public`` view

Step 3: Query the view
^^^^^^^^^^^^^^^^^^^^^^
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

Expose a subset of columns based on a role
------------------------------------------

Let's say we have a table called ``patient_information`` (id, doctor_id, is_confidential, confidential_information). We want the role ``doctor`` to be able to access the confidential information only if ``is_confidential`` is ``false`` or if their ``id`` equals the ``doctor_id`` of the table.

Step 1: Create a view 
^^^^^^^^^^^^^^^^^^^^^

Create a view called ``confidential_information`` (id, doctor_id, is_confidential, confidential_information):

.. code-block:: SQL

    CREATE VIEW confidential_information AS
      SELECT id, doctor_id, is_confidential, confidential_information
        FROM patient_information;

Step 2: Create a relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On the table ``patient_information``, create a manual object relationship from ``patient_information : id`` -> ``confidential_information : id`` called ``confidential_info``:

.. thumbnail:: ../../../../img/graphql/manual/auth/create-manual-relationship.png

Step 3: Define permissions
^^^^^^^^^^^^^^^^^^^^^^^^^^

For the role ``doctor``, create the following permissions for ``select``:

- Table ``patient_information``: the ``doctor`` can access patient information (except confidential information) if the ``id`` in the session variable is equal to ``doctor_id``
- View ``confidential_information``: the ``doctor`` can access confidential information either if ``is_confidential`` is ``false`` or if the ``id`` passed in the session variable is equal to ``doctor_id``

.. thumbnail:: ../../../../img/graphql/manual/auth/define-permissions-role-doctor.png

Step 3: Query the view
^^^^^^^^^^^^^^^^^^^^^^

Now we can query the view. We have to pass the ``X-Hasura-Role`` (in this case ``doctor``) and a valid ``X-Hasura-User-Id`` (in this case ``1``) as session variables.

.. graphiql::
  :view_only:
  :query:
    query {
      patient_information {
        is_confidential
        confidential_info {
          doctor_id
          confidential_information
        }
      }
    }
  :response:
    {
      "data": {
        "patient_information": [
          {
            "is_confidential": true,
            "confidential_info": {
              "doctor_id": 1,
              "confidential_information": "Has the flu"
            }
          },
          {
            "is_confidential": false,
            "confidential_info": {
              "doctor_id": 1,
              "confidential_information": "Broken arm"
            }
          }
        ]
      }
    }

