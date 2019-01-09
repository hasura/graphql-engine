Query a Custom SQL Function
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Custom SQL functions
--------------------
Custom SQL functions are user-defined functions that be used to encapsulate custom business logic that is not directly supported by or extends built-in functions or operators. Certain types of custom functions can now be exposed over GraphQL API for querying (``query`` and ``subscription``).

Supported SQL function or argument types
----------------------------------------
Currently, only functions which satisfy following constraints can be queried via GraphQL(*terminology from `Postgres docs <https://www.postgresql.org/docs/current/sql-createfunction.html/>`_*):

- **Function behaviour**: ONLY ``STABLE`` or ``IMMUTABLE``
- **Return type**: MUST be ``SETOF <table-name>`` (*for custom types, refer to :ref:`custom-types`*)
- **Argument types**: ONLY ``IN``

Creating & tracking SQL Functions
---------------------------------
Functions can be created using the ``Raw SQL`` section of the Hasura console or by using any PostgreSQL client and subsequently tracking the function:

- Head to the ``Data`` -> ``SQL`` section of Hasura console.
- Enter the function definition in the SQL window (*see `Postgres docs <https://www.postgresql.org/docs/current/sql-createfunction.html>`_ *)
- If the ``SETOF`` table already exists and is being tracked, check the ``Track this`` option and hit the ``Run`` button.

.. _custom-types:
Custom types
************

If your function needs to return a custom type i.e. a rowset , create an empty table instead and track it. You can do in the ``Raw SQL`` section in ``Data`` -> ``SQL``.

Using custom functions in GraphQL Query
---------------------------------------

In the usual example context of an articles/authors schema, let's say we've created and tracked a custom function, ``search_articles``, with the following definition: 

.. code-block:: sql

      CREATE FUNCTION search_articles(search text)
      returns SETOF article as $$
          select *
          from article
          where
          title ilike ('%' || search || '%') or
          content ilike ('%' || search || '%')
      $$ LANGUAGE sql STABLE;

This function filters rows from the ``article`` table based on the input text argument, ``search`` i.e. it returns ``SETOF article``. Assuming the ``article`` table is being tracked, you can use the custom function as follows:

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
            "title": "first post by hasura",
            "content": "some content for post"
          },
          {
            "id": 2,
            "title": "second post by hasura",
            "content": "some other content for post"
          }
        ]
      }
    }

.. note::

    1. You can query aggregations on a function result using ``<function-name>_aggregate`` field. E.g. Counting the number of articles returned by the above function:

    .. code-block:: graphql

          query {
            search_articles_aggregate(args: {search: "hasura"}}){
              aggregate {
                count
              }
            }
          }

    2. As with tables, arguments like ``where``, ``limit``, ``order_by``, ``offset``, etc. are also available for use with function-based queries. E.g. To limit the number of rows returned by query in the previous section:
    
    .. code-block:: graphql

          query {
            search_articles(args: {search: "hasura"}, limit: 5){
              id
              title
              content
            }
          }

Permissions for custom function queries
***************************************

Permissions configured for the **return table** of a function are also applicable to the function itself. E.g. if the role ``user`` doesn't have the requisite permissions to view the table ``article``, then the example query from the previous section will throw a validation error.