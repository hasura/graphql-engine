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

To convert an :doc:`insert mutation <insert>` into an upsert, you need to specify a unique or primary key constraint
and the columns to be updated in the case of a violation of that constraint using the ``on_conflict`` argument. You can
specify a constraint using the ``constraint`` field and choose the columns to update using the
``update_columns`` field of the argument. The value of the ``update_columns`` field determines the behaviour of the
upsert request in case of conflicts.

.. admonition:: Fetching Postgres constraint names

  You can fetch the name of unique or primary key constraints by querying the ``information_schema.table_constraints``
  table. GraphQL Engine will automatically generate constraint names as enum values for ``constraint`` (try
  autocompleting in GraphiQL). Typically, the constraint is automatically named as ``<table-name>_<column-name>_key``.


Update selected columns on conflict
-----------------------------------
Insert a new object in the author table or, if the primary key constraint, ``author_pkey``, is violated, update the columns
specified in ``update_columns``:

.. graphiql::
  :view_only:
  :query:
    mutation upsert_author {
      insert_author(
        objects: [
          {name: "John", id: 10}
        ],
        on_conflict: {
          constraint: author_pkey,
          update_columns: [name]
        }
      ) {
        affected_rows
        returning{
          id
          name
        }
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "affected_rows": 1,
          "returning": [
             {
               "name": "John",
               "id": 10
             }
           ]
        }
      }
    }

Ignore request on conflict
--------------------------
If ``update_columns`` is an **empty array** then GraphQL Engine ignore changes on conflict. Insert a new object into
the author table or, if the unique constraint, ``author_name_key``, is violated, ignore the request

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

In this case, the insert mutation is ignored because there is a conflict.


Upsert in nested mutations
--------------------------
You can specify ``on_conflict`` clause while inserting nested objects


.. graphiql::
  :view_only:
  :query:
    mutation upsert_author_article {
      insert_author(
        objects: [
          { name: "John",
            id: 10,
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


.. note::

  Inserting nested objects fails when:

  - Any of upsert in object relationships does not affect any rows (``update_columns: []``)
  - Array relationships are queued for insert and parent insert does not affect any rows (``update_columns: []``)
