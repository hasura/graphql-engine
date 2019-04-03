Soft deletes
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Some projects may require records to be "soft deleted". This will not actually remove items from your database, but mark them with a timestamp to indicate when they were deleted.

A common approach is to add a column such as `deleted_at` which is a nullable timestamp. When
there is a timestamp value, it should be excluded from queries by default.

Updating the query using permissions
------

Typically certain roles should not be allowed to query *soft deleted* records. To achieve this, we add a user role and modify the select query with a custom check.

.. thumbnail:: ../../../img/graphql/manual/auth/soft-deletes-user-role.png

Query records using the role
------

.. graphiql::
  :view_only:
  :query:
    query {
      todos {
        id
        name
        deleted_at
        created_at
        updated_at
      }
    }
  :response:
    {
      "data": {
        "todos": [
          "id": "0f980ce9-4611-46c8-ae4a-dcd7e6ea2e5a",
          "name": "Get stuff done",
          "deleted_at": null,
          "created_at": "2019-04-02T23:28:47.526386+00:00",
          "updated_at": "2019-04-02T23:28:47.526386+00:00"
        ]
      }
    }
