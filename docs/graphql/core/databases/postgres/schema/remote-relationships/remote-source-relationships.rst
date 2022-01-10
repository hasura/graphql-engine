.. meta::
   :description: Adding remote source relationships with Postgres tables in Hasura
   :keywords: hasura, docs, remote source relationship, remote join, remote source, data federation

.. _pg_remote_source_relationships:

Postgres: Remote source relationships
=====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Remote source relationships extend the concept of joining data between tables within a single database, to joining across tables between separate databases. Once you create relationships between
types from your source database to types from your target database, you can then "join" them by running GraphQL queries.

.. admonition:: Supported from

  Remote source relationships are supported from versions ``v2.1.0`` and above.

Create remote source relationships
----------------------------------

Step 0: Add two database sources
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Add a database source as described :ref:`here <connect_database>` and track the required tables. Then,
repeat the process to add your target database source.

Step 1: Define and create the relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A remote source relationship is defined alongside the source database table (that is,
the source side of the join). The following data is required to define a remote source relationship:

- **Name**: A name for the relationship.
- **Remote Source**: The name of the target database source (that is, the target side of the join)
- **Remote Table**: The table in the target database source that should be joined with the source table
- **Relationship type**: Either ``object`` or ``array`` - just as for normal relationships, Hasura supports
  both many-to-one (object) and one-to-many (array) relationships.
- **Field Mapping**: A mapping between fields in the source table and their corresponding fields in the
  target table, just as a foreign key relationship would be defined by such a mapping within a single database.

For this example, we assume that our source ``orders_db`` database has an ``orders`` table with the fields ``id`` and ``ordered_by_user_id``, and
that the target ``users_db`` database has a ``users`` table with the fields ``id`` and ``name``.

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
    "type": "create_remote_relationship",
    "args": {
      "name": "ordered_by_user",
      "source": "orders_db",
      "table": "orders",
      "remote_source": {
        "relationship_type": "object",
        "field_mapping": {
          "ordered_by_user_id": "id"
        },
        "source": "users_db",
        "table": "users"
      }
    }
  }


In this example, we've added a target database which contains our user information, and
then joined that with order information in our source database.

1. We name the relationship ``ordered_by_user``.
2. We select the ``users_db`` database as the target (or remote source)
3. We set up the config to join the ``id`` input argument of our remote source field to the ``ordered_by_user_id`` column of this table (in this case, the ``orders`` table).

Step 2: Explore with GraphiQL
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the GraphiQL tab, test out your remote source relationship.

.. graphiql::
  :view_only:
  :query:
    query {
      orders {
        id
        ordered_by_user {
          id
          name
        }
      }
    }
  :response:
    {
      "data": {
        "orders": [
          {
            "id": "2a34eda4",
            "ordered_by_user": {
              "id": "1d794fc7",
              "name": "Daenerys Targaryen"
            }
          }
        ]
      }
    }