.. meta::
   :description: Use upsert mutations with Hasura
   :keywords: hasura, docs, mutation, upsert

.. _upsert:

Upsert mutation
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

An upsert query will insert an object into the database in case there is no conflict with another row in the table. In
case there is a conflict with one or more rows, it will either update the fields of the conflicted rows or ignore
the request.

Convert insert mutation to upsert
---------------------------------

.. note::

  Only tables with **update** permissions are **upsertable**. i.e. a table's update permissions are respected
  before updating an existing row in case of a conflict.

To convert an :ref:`insert mutation <insert>` into an upsert, you need to use the ``on_conflict`` argument to specify:

- a **unique or primary key constraint** using the ``constraint`` field, and
- the **columns to be updated** in the case of a violation of that constraint using the ``update_columns`` field.

The value of the ``update_columns`` field determines the behaviour of the upsert request as shown via the use cases
below.

.. admonition:: Fetching Postgres constraint names

  You can fetch details of unique or primary key constraints on a table by running the following SQL:

  .. code-block:: sql

    SELECT * FROM "information_schema"."table_constraints" WHERE table_name='<table>' AND table_schema='<schema>';

  GraphQL engine will automatically generate constraint names as enum values for the ``constraint`` field *(try
  autocompleting in GraphiQL)*. Typically, the constraint is automatically named as ``<table-name>_<column-name>_key``.

Upsert is not a substitute for update
-------------------------------------

The upsert functionality is sometimes confused with the update functionality. However, they work slightly
differently. An upsert mutation is used in the case when it's not clear if the respective row is already present
in the database. If it's known that the row is present in the database, ``update`` is the functionality to use.

For an upsert, **all columns that are necessary for an insert are required**.

**How it works**

1. Postgres tries to insert a row (hence all the required columns need to be present)

2. If this fails because of some constraint, it updates the specified columns

If not all required columns are present, an error like ``NULL value unexpected for <not-specified-column>`` can occur.


Update selected columns on conflict
-----------------------------------
Insert a new object in the ``article`` table or, if the primary key constraint ``article_pkey`` is violated, update
the columns specified in ``update_columns``:

.. graphiql::
  :view_only:
  :query:
    mutation upsert_article {
      insert_article (
        objects: [
          {
            id: 2,
            title: "ex quis mattis",
            content: "Pellentesque lobortis quam non leo faucibus efficitur",
            published_on: "2018-10-12"
          }
        ],
        on_conflict: {
          constraint: article_pkey,
          update_columns: [title, content]
        }
      ) {
        returning {
          id
          title
          content
          published_on
        }
      }
    }
  :response:
    {
      "data": {
        "insert_article": {
          "returning": [
            {
              "id": 2,
              "title": "ex quis mattis",
              "content": "Pellentesque lobortis quam non leo faucibus efficitur",
              "published_on": "2018-06-10"
            }
          ]
        }
      }
    }

The ``published_on`` column is left unchanged as it wasn't present in ``update_columns``.

Update selected columns on conflict using a filter
--------------------------------------------------
Insert a new object in the ``article`` table, or if the primary key constraint ``article_pkey`` is violated, update
the columns specified in ``update_columns`` only if the provided ``where`` condition is met:


.. graphiql::
  :view_only:
  :query:
    mutation upsert_article {
      insert_article (
        objects: [
          {
            id: 2,
            published_on: "2018-10-12"
          }
        ],
        on_conflict: {
          constraint: article_pkey,
          update_columns: [published_on],
          where: {
            published_on: {_lt: "2018-10-12"}
          }
        }
      ) {
        returning {
          id
          published_on
        }
      }
    }
  :response:
    {
      "data": {
        "insert_article": {
          "returning": [
            {
              "id": 2,
              "published_on": "2018-10-12"
            }
          ]
        }
      }
    }

The ``published_on`` column is updated only if the new value is greater than the old value.

Ignore request on conflict
--------------------------
If ``update_columns`` is an **empty array** then the GraphQL engine ignores changes on conflict. Insert a new object into
the author table or, if the unique constraint ``author_name_key`` is violated, ignore the request.

.. graphiql::
  :view_only:
  :query:
    mutation upsert_author {
      insert_author(
        objects: [
          {name: "John", id: 10}
        ],
        on_conflict: {
          constraint: author_name_key,
          update_columns: []
        }
      ) {
        affected_rows
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "affected_rows": 0
        }
      }
    }

In this case, the insert mutation is ignored because there is a conflict and ``update_columns`` is empty.


Upsert in nested mutations
--------------------------
You can specify the ``on_conflict`` clause while inserting nested objects:

.. graphiql::
  :view_only:
  :query:
    mutation upsert_author_article {
      insert_author(
        objects: [
          {
            id: 10,
            name: "John",
            articles: {
              data: [
                {
                  id: 1,
                  title: "Article 1 title",
                  content: "Article 1 content"
                }
              ],
              on_conflict: {
                constraint: article_pkey,
                update_columns: [title, content]
              }
            }
          }
        ]
      ) {
        affected_rows
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "affected_rows": 2
        }
      }
    }


.. _nested-upsert-caveats:

Nested upsert caveats
^^^^^^^^^^^^^^^^^^^^^

.. note::

  The process by which nested inserts/upserts are executed is documented :ref:`here <nested_inserts>`.

  Nested upserts will fail when:

  - In case of an array relationship, the parent upsert does not affect any rows (i.e. ``update_columns: []`` for parent
    and a conflict occurs), as the array relationship objects are inserted after the parent.
  - In case of an object relationship, the nested object upsert does not affect any row (i.e. ``update_columns: []`` for
    nested object and a conflict occurs), as the object relationship object is inserted before the parent.

  To allow upserting in these cases, set ``update_columns: [<conflict-columns>]``. By doing this, in case of a
  conflict, the conflicted column/s will be updated with the new value (which is the same values as they had before and hence
  will effectively leave them unchanged) and will allow the upsert to go through.
