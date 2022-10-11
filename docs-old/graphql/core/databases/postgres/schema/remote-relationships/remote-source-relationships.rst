.. meta::
   :description: Adding remote database relationships with Postgres tables in Hasura
   :keywords: hasura, docs, remote database relationship, remote join, remote source, data federation

.. _pg_remote_source_relationships:

Postgres: Remote database relationships
=======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Remote database relationships (a.k.a remote source relationships) extend the concept of joining data between tables within a single database to joining data across tables between separate databases.

After you've established relationships between types in your source database and types in your target database, you can "join" them with GraphQL queries.

.. admonition:: Supported from

  Remote database relationships are supported from versions ``v2.1.0`` and above.

Create remote database relationships
------------------------------------

Step 0: Add two database sources
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Add a source database as described :ref:`here <connect_database>` and track the required tables. Then, repeat the process to add your target database.

Step 1: Define and create the relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A remote database relationship is defined alongside the source database table (that is, the source side of the join).

The following fields can be defined for a remote schema relationship:

- **Relationship type**: Either ``object`` or ``array`` - similar to normal relationships. Hasura supports both many-to-one (object) and one-to-many (array) relationships.
- **Relationship Name**: A name for the relationship.
- **Reference Source**: The name of the target database (that is, the target side of the join)
- **Reference Table**: The table in the target database source that should be joined with the source table
- **Field Mapping**: A mapping between fields in the source table and their corresponding fields in the target table, just as a foreign key relationship would be defined by such mapping within a single database.

For example, say we have a table ``orders(id int, order_user_id int)`` in the source database and a table ``users(id int, name text)`` in the target database.

We can create an object remote database relationship ``order_user`` joining the ``orders`` table to the ``users`` table using the ``orders.order_user_id`` and ``users.id`` fields.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Head to the ``Data > [database] > [orders] > Relationships`` tab. Under the ``Remote Database Relationships`` section, select ``Add a remote database relationship``.

    .. thumbnail:: /img/graphql/core/databases/postgres/schema/create-remote-source-relationship.png
       :alt: Create remote database relationships

    Hit ``Save``.

  .. tab:: CLI

    Update the ``metadata > databases > [db-name] > tables > [public_orders].yaml`` file:
  
    .. code-block:: yaml
       :emphasize-lines: 4-14

       table:
         name: orders
         schema: public
       remote_relationships:
       - name: order_user
         definition:
           to_source:
             relationship_type: object
             source: pg2
             table:
               name: users
               schema: public
             field_mapping:
               order_user_id: id

    Apply the metadata:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

    You can add a remote database relationship by using the :ref:`metadata_pg_create_remote_relationship` or
    :ref:`metadata_pg_update_remote_relationship` metadata APIs with the ``to_source`` field.

    .. code-block:: http

       POST /v1/metadata HTTP/1.1
       Content-Type: application/json
       X-Hasura-Role: admin
     
       {
         "type": "pg_create_remote_relationship",
         "args": {
           "name": "order_user",
           "source": "pg1",
           "table": {
             "name": "orders",
             "schema": "public"
           },
           "definition": {
             "to_source": {
               "relationship_type": "object",
               "source": "pg2",
               "table": {
                 "name": "users",
                 "schema": "public"
               },
               "field_mapping": {
                 "order_user_id": "id"
               }
             }
           }
         }
       }          


Step 2: Explore with GraphiQL
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Run the following query in the GraphiQL editor to test your remote database relationship across the two connected databases:

.. graphiql::
  :view_only:
  :query:
    query {
      orders {
        id
        order_user_id
        order_user {
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
            "id": 1,
            "order_user_id": 1,
            "order_user": {
              "user_id": 1,
              "name": "Torvalds"
            }
          },
          {
            "id": 2,
            "order_user_id": 2,
            "order_user": {
              "id": 2,
              "name": "Zuckerberg"
            }
          },
          {
            "id": 3,
            "order_user_id": 1,
            "order_user": {
              "id": 1,
              "name": "Torvalds"
            }
          },
          {
            "id": 4,
            "order_user_id": 3,
            "order_user": {
              "id": 3,
              "name": "Gates"
            }
          }
        ]
      }
    }
