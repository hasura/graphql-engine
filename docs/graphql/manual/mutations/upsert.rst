Upsert mutation
===============

To convert an **insert** mutation into an **upsert** one, you need to specify the unique or primary key constraint(s) and the
columns to be updated in the case of a conflict or violation. You can specify a constraint using the ``constraint`` argument and
update columns using the ``update_columns`` argument.

.. note::
    
    You can fetch the name of unique or primary key constraints by querying the ``information_schema.table_constraints`` table.
    GraphQL Engine will automatically generate constraint names as enum values for ``constraint`` (try autocompleting in GraphiQL).
    Typically, the constraint is automatically named as ``<table-name>_<column-name>_key``. 


Without "update_columns" argument
---------------------------------
When you don't explicitly specify ``update_columns``, the columns that are given in objects are updated (it doesn't matter if they
are different, you should see the same end result).

Insert into ``author`` table using unique constraint ``author_name_key``. All columns specified in objects get updated:

.. graphiql::
  :view_only:
  :query:
    mutation upsert_author {
      insert_author(
        objects: [
          {name: "John", age: 25, mobile: 9876543210}
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
          age
          mobile
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
               "id": 10,
               "name": "John",
               "age": 25,
               "mobile": 9876543210
             }
           ]
        }
      }
    }

**Note:** You'll need to ensure that all objects have the same set of columns. If not, the union of column sets across all objects
is the set of columns that is updated. For example, if your query as follows:

.. graphiql::
  :view_only:
  :query:
    mutation upsert_user {
      insert_user(
        objects: [
          { name: "John", email_sent: true, is_premium: true },
          { name: "Jack", email_sent: true }
        ]
        on_conflict: { constraint: user_name_key }
      ){
        affected_rows
        returning{
          name
          email_sent
          is_premium
        }
      }
    }
  :response:
    {
      "data": {
        "insert_user": {
          "affected_rows": 2,
          "returning": [
             {
               "name": "John",
               "email_sent": true,
               "is_premium": true
             },
             {
               "name": "Jack",
               "email_sent": true,
               "is_premium": false
             }
           ]
        }
      }
    }

The column ``"is_premium"`` for the ``"Jack"`` row is set to its ``DEFAULT`` value because the union of all columns across objects
is ``{name, email_sent, is_premium}``. However, you can explicitly control the columns that are updated on conflict using
``update_columns`` as specified in the following section.

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


Using "action" argument
-----------------------

.. note::
   ``action`` argument is deprecated. Always ``update_columns`` will take precedence over ``action`` argument

On conflict, you can choose to either ignore the mutation (``action: ignore``) or update the row that caused the conflict
(``action: update``). ``ignore`` and ``update`` are enum values for ``action``.

For the following examples, assume there's a unique constraint on the ``name`` column of the ``author`` table.

With constraint name and update
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Insert a new object in the author table or, if the unique constraint, ``author_name_key``, is violated, update
the columns that are given in objects:

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
          action: update 
        }
      ) {
        affected_rows
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "affected_rows": 1
        }
      }
    }

The response shown above assumes that the name of the author in our object is not unique and then
*updates* the corresponding row in the database.

With constraint name and ignore
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Insert a new object into the author table or, if the unique constraint, ``author_name_key``, is violated,
ignore the request:

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
          action: ignore
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

   1. Any of upsert in object relationships does not affect any rows (``update_columns: []`` or ``action: ignore``)

   2. Array relationships are queued for insert and parent insert does not affect any rows (``update_columns: []`` or ``action: ignore``)
