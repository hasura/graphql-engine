Computed fields
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

What are computed fields?
-------------------------

Computed fields are virtual values or objects that are dynamically computed and can be queried along with a table's
columns. When requested for, computed fields are computed via SQL functions using the other columns of the table and
any user inputs if needed.

.. note::

  Computed fields are only exposed over the GraphQL API and the database schema is not modified on addition of a
  computed field.

Supported SQL functions
-----------------------

Only functions which satisfy the following constraints can be added as a computed field to a table.
(*terminology from* `Postgres docs <https://www.postgresql.org/docs/current/sql-createfunction.html>`__):

- **Function behaviour**: ONLY ``STABLE`` or ``IMMUTABLE``
- **Argument modes**: ONLY ``IN``
- **Table Argument**: One input argument with a table row type
- **Return type**: Either ``SETOF <table-name>`` or ``BASE`` type

Defining computed fields
------------------------

Based on the SQL function's return type, we can define two types of computed fields:

1. Scalar computed fields
*************************

Computed fields whose associated SQL function returns a
`base type <https://www.postgresql.org/docs/current/extend-type-system.html#id-1.8.3.5.9>`__ like *Integer*,
*Boolean*, *Geography* etc.

**Example:**

The ``author`` table has two ``text`` columns: ``first_name`` and ``last_name``.

Define an SQL function called ``author_full_name``:

.. code-block:: plpgsql

  CREATE FUNCTION author_full_name(author_row author)
  RETURNS TEXT AS $$
    SELECT author_row.first_name || ' ' || author_row.last_name
  $$ LANGUAGE sql STABLE;

Add a computed field called ``full_name`` to the ``author`` table using the SQL function above.
See :doc:`API Reference <../api-reference/schema-metadata-api/computed-field>`.

Query data from the ``author`` table:

.. graphiql::
  :view_only:
  :query:
    query {
      author {
        id
        first_name
        last_name
        full_name
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 1,
            "first_name": "Chris",
            "last_name": "Raichael",
            "full_name": "Chris Raichael"
          }
        ]
      }
    }

2. Table computed fields
************************

Computed fields whose associated SQL function returns ``SETOF <table-name>`` are table computed fields.
The return table must be tracked to define such a computed field.

**Example:**

In a simple ``author <-> article`` schema, we can define a :doc:`relationship <../schema/relationships/index>` on the ``author``
table to fetch authors along with their articles. We can make use of computed fields to fetch the author's articles
by search.

Define an SQL function called ``fetch_articles``:

.. code-block:: plpgsql

   CREATE FUNCTION fetch_articles(search text, author_row author)
   RETURNS SETOF article AS $$
     SELECT *
     FROM article
     WHERE
       ( title ilike ('%' || search || '%')
         OR content ilike ('%' || search || '%')
       ) AND author_id = author_row.id
   $$ LANGUAGE sql STABLE;

Add a computed field called ``get_articles`` to the ``author`` table using the SQL function above.
See :doc:`API Reference <../api-reference/schema-metadata-api/computed-field>`.

Query data from the ``author`` table:

.. graphiql::
  :view_only:
  :query:
    query {
      author {
        id
        first_name
        last_name
        get_articles(args: {search: "Hasura"}){
          id
          title
          content
        }
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 1,
            "first_name": "Chris",
            "last_name": "Raichael",
            "get_articles": [
              {
                "id": 1,
                "title": "Computed fields in Hasura",
                "content": "lorem ipsum dolor sit amet"
              }
            ]
          }
        ]
      }
    }

Computed fields vs. Postgres generated columns
----------------------------------------------

Postgres, from version ``12``, is introducing `Generated Columns <https://www.postgresql.org/docs/12/ddl-generated-columns.html>`__.
The value of generated columns is also computed from other columns of a table. Postgres' generated columns
come with their own limitations. Hasura's computed fields are defined via an SQL function, which allows users
to define any complex business logic in a function. Generated columns will go together with computed fields where
Hasura treats generated columns as normal Postgres columns.
