Upsert mutation
===============

To convert an **insert** mutation into an **upsert** one, you need to specify the unique or primary key constraint(s) and the
columns to be updated in the case of a conflict or violation. You can specify a constraint using the ``constraint`` argument and
update columns using the ``update_columns`` argument.

.. note::
    
    #. Only tables with **update** permissions are **upsertable**.
       Learn more about permissions :doc:`here <../api-reference/schema-metadata-api/permission>`.
    #. You can fetch the name of unique or primary key constraints by querying the ``information_schema.table_constraints`` table.
       GraphQL Engine will automatically generate constraint names as enum values for ``constraint`` (try autocompleting in GraphiQL).
       Typically, the constraint is automatically named as ``<table-name>_<column-name>_key``.


With non empty "update_columns"
-------------------------------
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


With empty "update_columns"
---------------------------
If ``update_columns`` is an empty array then GraphQL Engine ignore changes on conflict. Insert a new object into the author
table or, if the unique constraint, ``author_name_key``, is violated, ignore the request

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


.. warning::
   Inserting nested objects fails when

   1. Any of upsert in object relationships does not affect any rows (``update_columns: []``)

   2. Array relationships are queued for insert and parent insert does not affect any rows (``update_columns: []``)
