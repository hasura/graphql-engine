Query a Custom SQL Function
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Custom SQL functions can be exposed over GraphQL API for querying.

Function Limitations
--------------------
Only functions which satisfy following constraints are allowed

- ``VARIADIC`` parameters are not supported
- Only ``STABLE`` or ``IMMUTABLE`` type allowed for queries
- Return type must be ``SETOF`` table name which is **tracked**

GraphQL Query
-------------
``search_articles`` SQL function filters all articles based on input ``search`` argument. It returns ``SETOF article``.
``article`` table must be added to Hasura metadata either through console or using
:doc:`metadata API <../api-reference/schema-metadata-api/table-view>`.

search_articles function definition:

.. code-block:: sql

      CREATE FUNCTION search_articles(search text)
      returns SETOF article as $$
          select *
          from article
          where
          title ilike ('%' || search || '%') or
          content ilike ('%' || search || '%')
      $$ LANGUAGE sql STABLE;

Query ``search_articles`` function:

.. graphiql::
  :view_only:
  :query:
    query {
      search_articles(args: {search: "hasura"}){
        id
        title
        content
      }
    }
  :response:
    {
      "data": {
        "search_articles": [
          {
            "id": 1,
            "title": "post by hasura",
            "content": "content for post"
          },
          {
            "id": 2,
            "title": "second post by hasura",
            "content": "content for post"
          }
        ]
      }
    }

Apply ``limit`` argument:

.. graphiql::
  :view_only:
  :query:
    query {
      search_articles(args: {search: "hasura"}, limit: 1){
        id
        title
        content
      }
    }
  :response:
    {
      "data": {
        "search_articles": [
          {
            "id": 1,
            "title": "post by hasura",
            "content": "content for post"
          }
        ]
      }
    }

.. note::

    1. You can query aggregations on a function result using ``<function-field-name>_aggregate`` field
    2. ``where``, ``limit``, ``order_by`` and ``offset`` arguments are available on function queries
    3. Permissions and relationships on **return table** of function are considered in function queries

Creating SQL Functions
----------------------

Functions can be created using SQL which can be run in the Hasura console:

- Head to the ``Data -> SQL`` section of Hasura console
- Enter you `create function SQL statement <https://www.postgresql.org/docs/current/sql-createfunction.html>`__
- Hit the ``Run`` button
- Add function to metadata using Hasura console or :doc:`API <../api-reference/schema-metadata-api/custom-functions>`
