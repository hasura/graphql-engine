Computed Columns
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

What are computed columns?
--------------------------

Computed columns are extra fields added to a table whose value is computed from other columns.
It facilitates users to define an additional field to an existing table whose
return value is computed via an SQL function. Computed columns are exposed only over the GraphQL API. The server
does not modify the database schema to define a computed column. Computed columns are added as a part of
metadata.

Supported SQL functions
***********************

Only functions which satisfy the following constraints can be added as a computed column to a table.

- **Function behaviour**: ONLY ``STABLE`` or ``IMMUTABLE``
- **Argument modes**: ONLY ``IN``
- **Table Argument**: One input argument with table row type
- **Return type**: Either ``SETOF <table-name>`` or ``BASE`` type

Defining computed columns
-------------------------

Based on the SQL function return type we can define two types of computed columns

1. Scalar computed columns
**************************

Computed columns whose associated SQL function returns
`Base type <https://www.postgresql.org/docs/current/extend-type-system.html#id-1.8.3.5.9>`__ like *Integer*,
*Boolean*, *Geography* etc.

**Example:-**

``author`` table has two ``text`` columns, ``first_name`` and ``last_name``. Define an SQL function ``author_full_name``.

.. code-block:: plpgsql

  CREATE FUNCTION author_full_name(author_row author)
  RETURNS TEXT AS $$
    SELECT author_row.first_name || ' ' || author_row.last_name
  $$ LANGUAGE sql STABLE;

Add a computed column ``full_name`` to ``author`` table using the SQL function above.
See :doc:`API Reference <../api-reference/schema-metadata-api/computed-column>`

Query data from ``author`` table

.. graphiql::
  :view_only:
  :query:
    {
      author{
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

2. Table computed columns
*************************

Computed columns whose associated SQL functions returns ``SETOF <table-name>`` are table computed columns.
Return table must be tracked to define such computed column.

**Example:-**

In a simple ``author <-> article`` schema we can define :doc:`relationship <../schema/relationships/index>` on ``author``
table to fetch authors along with their articles. We can make use of computed columns to fetch author's articles
with search.

Define ``fetch_articles`` SQL function.

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

Add a computed column ``get_articles`` to ``author`` table using the SQL function above.
See :doc:`API Reference <../api-reference/schema-metadata-api/computed-column>`

Query data from ``author`` table

.. graphiql::
  :view_only:
  :query:
    {
      author{
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
                "title": "Computed columns in Hasura",
                "content": "Some content related to computed columns"
              }
            ]
          }
        ]
      }
    }

How a computed column is different from Postgres generated column?
---------------------------------------------------------------

Postgres, from version ``12``, is introducing `Generated Columns <https://www.postgresql.org/docs/12/ddl-generated-columns.html>`__.
The value of generated columns is also computed from other columns of a table. Postgres' generated columns
come with its own limitations. Hasura's computed columns are defined via an SQL function, which allows users
to define any complex business logic in a function. Generated columns will go together with computed columns where
Hasura treats generated columns as normal postgres columns.
