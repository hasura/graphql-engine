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

Introduction
------------

A ``one-to-one`` relationship between two tables can be established via a **unique foreign key constraint**.

Say we have the following two tables in our database schema:

.. code-block:: sql

  authors (
    id SERIAL PRIMARY KEY,
    name TEXT
  )

  passport_info (
    id SERIAL PRIMARY KEY,
    owner_id INT NOT NULL
    passport_number TEXT
    ...
  )

These two tables are related via a ``one-to-one`` relationship. i.e.:

- an ``author`` can have one ``passport_info``
- a ``passport_info`` has one ``owner``

Step 1: Set up a table relationship in the database
---------------------------------------------------

This ``one-to-one`` relationship can be established in the database by:

1. Adding a **foreign key constraint** from the ``passport_info`` table to the ``authors`` table using the ``owner_id``
   and ``id`` columns of the tables respectively
2. Adding a **unique constraint** to the ``owner_id`` column for the ``passport_info`` table


This will ensure that the value of the ``owner_id`` column in ``passport_info`` table  is present in the ``id`` column of
the ``authors`` table and there will be only one row with a particular ``owner_id``.

Step 2: Set up GraphQL relationships
------------------------------------

To access the nested objects via the GraphQL API, :ref:`create the following relationships <pg_create_relationships>`:

- Object relationship, ``passport_info`` from the ``authors`` table using  ``id -> passport_info :: owner_id``
- Object relationship, ``owner`` from the ``passport_info`` table using ``owner_id -> authors :: id``

Query using one-to-one relationships
------------------------------------

We can now:

- fetch a list of ``authors`` with their ``passport_info``:

  .. graphiql::
    :view_only:
    :query:
        query {
          authors {
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
          "authors": [
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


- fetch a list of ``passport_infos`` with their ``owner``:

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

.. _one-to-one-insert:

Insert using one-to-one relationships
-------------------------------------

We can now:
 
- insert ``passport_info`` with their ``owner`` where the ``owner`` might already exist (assume unique ``name`` for ``owner``):
 
.. graphiql::
  :view_only:
  :query:
    mutation upsertPassportInfoWithOwner {
      insert_passport_info(objects: [
        {
          passport_number: "X98973765",
          owner: {
            data: {
              name: "Kelly"
            },
            on_conflict: {
              constraint: owner_name_key,
              update_columns: [name]
            }
          },
        }
      ]) {
        returning {
          passport_number
          owner {
            name
          }
        }
      }
    }
  :response:
    {
      "data": {
        "insert_passport_info": {
          "returning": [
            {
              "passport_number": "X98973765",
              "owner": {
                "name": "Kelly"
              }
            }
          ]
        }
      }
    }
 
.. note::
 
 You can avoid the ``on_conflict`` clause if you will never have conflicts.

Caveat for nested inserts
^^^^^^^^^^^^^^^^^^^^^^^^^

Due to the way nested inserts are typically handled (described :ref:`here <pg_nested_inserts>`),
the order of object insertion needs to be specified using the :ref:`insertion_order <ObjRelUsingManualMapping>` option while
creating one-to-one relationships via the API. This is necessary to ensure nested inserts are possible
using either side as the parent which would otherwise error out with a ``Not-NULL violation`` error in one of the cases.

In our example, inserting a ``passport_info`` with their nested ``owner`` will work seamlessly but trying to
insert an ``author`` with their nested ``passport_info`` will throw a constraint violation error in case the insertion order is
not specified for the ``owner`` object relationship.

