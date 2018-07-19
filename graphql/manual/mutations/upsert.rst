Upsert mutation
===============

To convert an **insert** mutation into an **upsert** one, you need to specify the unique or primary key constraint(s) and the action
to be taken in the case of a conflict or violation. You can specify a constraint using the ``constriant`` argument.
On conflict, you can choose to either ignore the mutation (``action: ignore``) or update the row that caused the conflict (``action: update``).
``ignore`` and ``update`` are enum values for ``action``.
For the following examples, assume there's a unique constraint on the ``name`` column of the ``author`` table.

.. note::
    
    You can fetch the name of unqiue or primary key constraints by quering the ``information_schema.table_constraints`` table.
    GraphQL Engine will automatically generate constraint names as enum values for ``constraint`` (try autocompleting in GraphiQL).
    Typically, the constraint is automatically named as ``<table-name>_<column-name>_key``. 

With constraint name and update
------------------------------------
Insert a new object in the author table or, if the unique constraint, ``author_name_key``, is violated, update
the existing object:

.. graphiql::
  :view_only: true
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
------------------------------------
Insert a new object into the author table or, if the unique constraint, ``author_name_key``, is violated,
ignore the request:

.. graphiql::
  :view_only: true
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

.. note::

  ``constraint`` is optional when ``action`` is ``ignore``.
