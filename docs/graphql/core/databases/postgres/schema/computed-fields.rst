.. meta::
   :description: Use computed fields over Postgres in Hasura
   :keywords: hasura, docs, postgres, schema, computed field

.. _computed_fields:

Postgres: Computed fields
=========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

What are computed fields?
-------------------------

Computed fields are virtual values or objects that are dynamically computed and can be queried along with a table/view's
columns. Computed fields are computed when requested for via `custom SQL functions <https://www.postgresql.org/docs/current/sql-createfunction.html>`__
(a.k.a. stored procedures) using other columns of the table/view and other custom inputs if needed.

.. note::

  Computed fields are only exposed over the GraphQL API and the database schema is not modified on addition of a
  computed field.

Supported SQL functions
***********************

Only functions which satisfy the following constraints can be added as a computed field to a table/view.
(*terminology from* `Postgres docs <https://www.postgresql.org/docs/current/sql-createfunction.html>`__):

- **Function behaviour**: ONLY ``STABLE`` or ``IMMUTABLE``
- **Argument modes**: ONLY ``IN``
- **Table Argument**: One input argument with a table row type
- **Return type**: Either ``SETOF <table-name>`` or ``BASE`` type

.. note::

  Functions used as computed fields can also accept other arguments other than the mandatory table row argument.
  Values for these extra arguments can be passed as arguments to the computed field in the GraphQL API.

Computed field types
--------------------

Based on the SQL function's return type, we can define two types of computed fields:

1. Scalar computed fields
*************************

Computed fields whose associated SQL function returns a
`base type <https://www.postgresql.org/docs/current/extend-type-system.html#id-1.8.3.5.9>`__ like *Integer*,
*Boolean*, *Geography* etc. are scalar computed fields.

**Example:**

Let's say we have the following schema:

.. code-block:: plpgsql

  authors(id integer, first_name text, last_name text)

:ref:`Define an SQL function <create_sql_functions>` called ``author_full_name``:

.. code-block:: plpgsql

  CREATE FUNCTION author_full_name(author_row authors)
  RETURNS TEXT AS $$
    SELECT author_row.first_name || ' ' || author_row.last_name
  $$ LANGUAGE sql STABLE;

:ref:`Add a computed field <add-computed-field>` called ``full_name`` to the ``authors`` table using the SQL function above.

Query data from the ``authors`` table:

.. graphiql::
  :view_only:
  :query:
    query {
      authors {
        id
        first_name
        last_name
        full_name
      }
    }
  :response:
    {
      "data": {
        "authors": [
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

Let's say we have the following schema:

.. code-block:: plpgsql

  authors(id integer, first_name text, last_name text)

  articles(id integer, title text, content text, author_id integer)

Now we can define a :ref:`table relationship <table_relationships>` on the ``authors``
table to fetch authors along with their articles.

We can make use of computed fields to fetch the author's articles with a search parameter.

:ref:`Define an SQL function <create_sql_functions>` called ``filter_author_articles``:

.. code-block:: plpgsql

   CREATE FUNCTION filter_author_articles(author_row authors, search text)
   RETURNS SETOF articles AS $$
     SELECT *
     FROM articles
     WHERE
       ( title ilike ('%' || search || '%')
         OR content ilike ('%' || search || '%')
       ) AND author_id = author_row.id
   $$ LANGUAGE sql STABLE;

:ref:`Add a computed field <add-computed-field>` called ``filtered_articles`` to the ``authors`` table using the SQL function above.

Query data from the ``authors`` table:

.. graphiql::
  :view_only:
  :query:
    query {
      authors {
        id
        first_name
        last_name
        filtered_articles(args: {search: "Hasura"}){
          id
          title
          content
        }
      }
    }
  :response:
    {
      "data": {
        "authors": [
          {
            "id": 1,
            "first_name": "Chris",
            "last_name": "Raichael",
            "filtered_articles": [
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

.. _add-computed-field:

Adding a computed field to a table/view
---------------------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     Head to the ``Modify`` tab of the table/view and click on the ``Add`` button in the ``Computed fields``
     section:

     .. thumbnail:: /img/graphql/core/schema/computed-field-create.png

     .. admonition:: Supported from

       - Console support for tables is available in ``v1.1.0`` and above
       - Console support for views is available in ``v1.3.0`` and above

  .. tab:: CLI

    You can add a computed field in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
       :emphasize-lines: 4-11

        - table:
            schema: public
            name: authors
          computed_fields:
          - name: full_name
            definition:
              function:
                schema: public
                name: author_full_name
              table_argument: null
            comment: ""

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

     A computed field can be added to a table/view using the :ref:`add_computed_field metadata API <api_computed_field>`:

     .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "add_computed_field",
        "args": {
          "table": {
            "name": "authors",
            "schema": "public"
          },
          "name": "full_name",
          "definition": {
            "function": {
              "name": "author_full_name",
              "schema": "public"
            },
            "table_argument": "author_row"
          }
        }
      }

Computed fields permissions
---------------------------

:ref:`Access control <authorization>` to computed fields depends on the type of computed field.

- For **scalar computed fields**, permissions are managed similar to the :ref:`columns permissions <col-level-permissions>`
  of the table.

- For **table computed fields**, the permissions set on the return table are respected.


Accessing Hasura session variables in computed fields
-----------------------------------------------------

It can be useful to have access to the session variable from the SQL function defining a computed field.
For instance, suppose we want to record which users have liked which articles. We can do so using a table
``article_likes`` that specifies a many-to-many relationship between ``articles`` and ``users``. In such a
case it can be useful to know if the current user has liked a specific article, and this information can be
exposed as a *Boolean* computed field on ``articles``.

Create a function with an argument for session variables and add it with the :ref:`add_computed_field` API with the
``session_argument`` key set. The session argument is a JSON object where keys are session variable names
(in lower case) and values are strings.  Use the ``->>`` JSON operator to fetch the value of a session variable
as shown in the following example.

.. code-block:: plpgsql

      -- 'hasura_session' will be the session argument
      CREATE OR REPLACE FUNCTION article_liked_by_user(article_row articles, hasura_session json)
      RETURNS boolean AS $$
      SELECT EXISTS (
          SELECT 1
          FROM article_likes A
          WHERE A.user_id = hasura_session ->> 'x-hasura-user-id' AND A.article_id = article_row.id
      );
      $$ LANGUAGE sql STABLE;

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type":"add_computed_field",
       "args":{
           "table":{
               "name":"articles",
               "schema":"public"
           },
           "name":"liked_by_user",
           "definition":{
               "function":{
                   "name":"article_liked_by_user",
                   "schema":"public"
               },
               "table_argument":"article_row",
               "session_argument":"hasura_session"
           }
       }
   }

.. graphiql::
  :view_only:
  :query:
     query {
       articles(where: {id: {_eq: 3}}) {
         id
         liked_by_user
       }
     }
  :response:
    {
      "data": {
        "articles": [
          {
            "id": "3",
            "liked_by_user": true
          }
        ]
      }
    }

.. note::

   The specified session argument is not included in the argument options of the computed
   field in the GraphQL schema.

.. admonition:: Supported from

   This feature is available in ``v1.3.0`` and above

Computed fields vs. Postgres generated columns
----------------------------------------------

Postgres, from version ``12``, is introducing `Generated Columns <https://www.postgresql.org/docs/12/ddl-generated-columns.html>`__.
The value of generated columns is also computed from other columns of a table. Postgres' generated columns
come with their own limitations. Hasura's computed fields are defined via an SQL function, which allows users
to define any complex business logic in a function. Generated columns will go together with computed fields where
Hasura treats generated columns as normal Postgres columns.

Computed fields in Remote relationships
---------------------------------------

Using computed fields in :doc:`Remote relationships <remote-relationships/index>` allows transformation of data
from table columns before joining with data from remote sources. For instance, suppose we want to extract certain
field from a ``json`` column and join it with a field in a remote schema by argument value. We would define a computed
field which returns a scalar type of the field value in the ``json`` column and use it to join the graphql field of
the remote schema. Consider the following Postgres schema.

.. thumbnail:: /img/graphql/core/databases/postgres/schema/computed-fields-remote-relationship.png

.. code-block:: plpgsql

   CREATE TABLE "user" (id SERIAL PRIMARY KEY, name TEXT UNIQUE NOT NULL, address json NOT NULL);

   -- SQL function returns city of a "user" using "->>" json operator
   CREATE FUNCTION get_city(table_row "user")
   RETURNS TEXT AS $$
     SELECT table_row.address ->> 'city'
   $$ LANGUAGE sql STABLE;

Now, let's track the table and add computed field ``user_city`` using the SQL function ``get_city``. Consider the
following remote schema.

.. code-block:: graphql

     type Query {
       get_coordinates(city: String): Coordinates
     }
     type Coordinates{
       lat: Float
       long: Float
     }


:ref:`Define a remote relationship<create_remote_relationship>` with name ``user_location`` from ``user_city``
scalar computed field to ``get_coordinates`` remote schema field. We can query users with the pincode of their residing place.

.. graphiql::
  :view_only:
  :query:
    query {
      user {
        id
        name
        user_city
        user_location
      }
    }
  :response:
    {
      "data": {
        "authors": [
          {
            "id": 1,
            "name": "Alice",
            "user_city": "Frisco",
            "user_location": {
              "lat": 33.155373,
              "long": -96.818733
            }
          }
        ]
      }
    }

.. note::

   Only ``Scalar computed fields`` are allowed to join fields from remote sources

.. admonition:: Supported from

   This feature is available in ``v2.0.1`` and above
