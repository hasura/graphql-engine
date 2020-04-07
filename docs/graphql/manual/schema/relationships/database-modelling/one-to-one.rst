.. meta::
   :description: Model one-to-one relationships in Hasura
   :keywords: hasura, docs, schema, relationship, one-to-one, 1-1

.. _one_to_one_modelling:

Modelling one-to-one table relationships
========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

A ``one-to-one`` relationship between two tables can be established via a **unique foreign key constraint**.

Say we have the following two tables in our database schema:

.. code-block:: sql

  author (
    id INT PRIMARY KEY,
    name TEXT
  )

  passport_info (
    id INT PRIMARY KEY,
    owner_id INT NOT NULL
    passport_number TEXT
    ...
  )

These two tables are related via a ``one-to-one`` relationship. i.e.:

- an ``author`` can have one ``passport_info``
- a ``passport_info`` has one ``owner``

Set up a table relationship in the database
-------------------------------------------

This ``one-to-one`` relationship can be established in the database by:

1. Adding a **foreign key constraint** from the ``passport_info`` table to the ``author`` table using the ``owner_id``
   and ``id`` columns of the tables respectively
2. Adding a **unique constraint** to the ``owner_id`` column for the ``passport_info`` table


This will ensure that the value of the ``owner_id`` column in ``passport_info`` table  is present in the ``id`` column of
the ``author`` table and there will be only one row with a particular ``owner_id``.

Set up GraphQL relationships
----------------------------

To access the nested objects via the GraphQL API, :ref:`create the following relationships <create_relationships>`:

- Object relationship, ``passport_info`` from the ``author`` table using  ``id -> passport_info :: owner_id``
- Object relationship, ``owner`` from the ``passport_info`` table using ``owner_id -> author :: id``

Query using relationships
-------------------------

We can now:

- fetch a list of authors with their ``passport_info``:

  .. graphiql::
    :view_only:
    :query:
        query {
          author {
            id
            name
            passport_info {
              id
              passport_number
            }
          }
        }
    :response:
      {
        "data": {
          "author": [
            {
              "id": 1,
              "name": "Justin",
              "passport_info": {
                "id": 1,
                "passport_number": "987456234"
              }
            },
            {
              "id": 2,
              "name": "Beltran",
              "passport_info": {
                "id": 2,
                "passport_number": "F0004586"
              }
            }
          ]
        }
      }


- fetch a list of passport_infos with their ``owner``:

  .. graphiql::
    :view_only:
    :query:
        query {
          passport_info {
            id
            passport_number
            owner {
              id
              name
            }
          }
        }
    :response:
      {
        "data": {
          "passport_info": [
            {
              "id": 1,
              "passport_number": "987456234",
              "owner": {
                "id": 1,
                "name": "Justin"
              }
            },
            {
              "id": 2,
              "passport_number": "F0004586",
              "owner": {
                "id": 2,
                "name": "Beltran"
              }
            }
          ]
        }
      }

Current limitations with nested mutations
-----------------------------------------

With one-to-one relationships, currently nested mutations will work only in one of the two directions.

In our example, inserting a ``passport_info`` with their nested ``owner`` will work seamlessly but trying to
insert an ``author`` with their nested ``passport_info`` will throw a constraint violation error.

This is due to the way Hasura GraphQL engine currently handles nested mutations (described in detail
:ref:`here <nested_inserts>`). As nested object relations are inserted before the parent, the ``passport_info``
will be attempted to be inserted first and the value of its ``owner_id`` will be attempted to be set as the
``id`` of the ``author``. Due to this, based on whether the ``owner_id`` of ``passport_info`` is nullable or not, a
``Not-NULL violation`` error will be thrown either for the ``owner_id`` field of ``passport_info`` or the ``id``
field of ``author``.
