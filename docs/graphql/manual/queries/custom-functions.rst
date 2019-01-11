Query a Custom SQL Function
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Custom SQL functions
--------------------
Custom SQL functions are user-defined functions that be used to encapsulate custom business logic that is not directly supported by or extends built-in functions or operators. Certain types of custom functions can now be exposed over GraphQL API for querying (``query`` and ``subscription``).

Supported SQL function or argument types
----------------------------------------
Currently, only functions which satisfy following constraints can be queried via GraphQL(*terminology from* `Postgres docs <https://www.postgresql.org/docs/current/sql-createfunction.html/>`_):

- **Function behaviour**: ONLY ``STABLE`` or ``IMMUTABLE``
- **Return type**: MUST be ``SETOF <table-name>`` (*for custom types, refer to the next section*)
- **Argument types**: ONLY ``IN``

Creating & tracking SQL Functions
---------------------------------
Functions can be created using the ``Raw SQL`` section of the Hasura console or by using any PostgreSQL client and subsequently tracking the function:

- If the ``SETOF`` table doesn't already exist or your function needs to return a custom type i.e. a rowset, create an empty table to support the function and track it before following the rest of the instructions.
- Head to the ``Data`` -> ``SQL`` section of Hasura console.
- Enter the function definition in the SQL window (*see* `Postgres docs <https://www.postgresql.org/docs/current/sql-createfunction.html>`_)
- Check the ``Track this`` option and hit the ``Run`` button.

Using custom functions in GraphQL Query
---------------------------------------

Let's see how we can use custom functions in a GraphQL query by using two examples:

Example: Text-search functions
******************************

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

Example: PostGIS functions
**************************

Let's take a look at an example where the ``SETOF`` table is not part of the existing schema. Say you have 2 tables, for user and landmark location data, with the following definitions (*this example uses the popular spatial database extension,* `PostGIS <https://postgis.net/>`_):

.. code-block:: sql
      
      -- User location data
      CREATE TABLE user_location (
        user_id INTEGER PRIMARY KEY,
        location GEOGRAPHY(Point)
      );
      
      -- Landmark location data
      CREATE TABLE landmark (
        id SERIAL PRIMARY KEY,
        name TEXT,
        type TEXT,
        location GEOGRAPHY(Point)
      );

In this example, we want to fetch a list of landmarks that are near a given user, along with the user's details in the same query. PostGIS' built-in function ``ST_Distance`` can be used to implement this use case.

Since our use case requires an output that isn't a "subset" of any of the existing tables i.e. the ``SETOF`` table doesn't exist, let's first create this table and then create our location search function. Head to the ``Data`` -> ``SQL`` section of Hasura console and: 

- create the following table and track the table:

  .. code-block:: sql
      
      -- SETOF table

      CREATE TABLE user_landmarks (
        user_id INTEGER,
        location GEOGRAPHY(Point),
        nearby_landmarks JSON
      );

- create the following function and track the function:

  .. code-block:: sql

      -- function returns a list of landmarks near a user based on the 
      -- input arguments distance_kms and userid

      CREATE FUNCTION search_landmarks_near_user(userid integer, distance_kms integer)
      returns SETOF user_landmarks as $$
        SELECT  A.user_id, A.location,
        (select json_agg(row_to_json(B)) FROM landmark B WHERE (ST_Distance(
                ST_Transform(B.location::Geometry, 3857),
                ST_Transform(A.location::Geometry, 3857)
            ) /1000) < distance_kms) as nearby_landmarks
        FROM user_location A where A.user_id = userid
      $$ LANGUAGE sql STABLE;

This function fetches user information (*for the given input* ``userid``) and a list of landmarks which are less than ``distance_kms`` kilometers away from the user's location as a JSON field. We can now refer to this function in our GraphQL API as follows:

.. graphiql::
  :view_only:
  :query:
    query {
      search_landmarks_near_user(args:{userid:3,distance_kms:20})
      {
        user_id
        location
        nearby_landmarks
      }
    }
  :response:
    {
      "data": {
        "search_landmarks_near_user": [
          {
            "user_id": 3,
            "location": {
              "type": "Point",
              "crs": {
                "type": "name",
                "properties": {
                  "name": "urn:ogc:def:crs:EPSG::4326"
                }
              },
              "coordinates": [
                12.9406589,
                77.6185572
              ]
            },
            "nearby_landmarks": [
              {
                "id": 3,
                "name": "blue tokai",
                "type": "coffee shop",
                "location": "0101000020E61000004E74A785DCF22940BE44060399665340"
              },
              {
                "id": 4,
                "name": "Bangalore",
                "type": "city",
                "location": "0101000020E61000005396218E75F12940E78C28ED0D665340"
              }
            ]
          }
        ]
      }
    }

Aggregations on custom functions
********************************

You can query aggregations on a function result using ``<function-name>_aggregate`` field. E.g. Count the number of articles returned by the function in the first example above:

.. code-block:: graphql

      query {
        search_articles_aggregate(args: {search: "hasura"}}){
          aggregate {
            count
          }
        }
      }

Using arguments with custom functions
*************************************
As with tables, arguments like ``where``, ``limit``, ``order_by``, ``offset``, etc. are also available for use with function-based queries. E.g. Limit the number of rows returned by the query in the first example above:
    
.. code-block:: graphql

    query {
      search_articles(args: {search: "hasura"}, limit: 5){
        id
        title
        content
      }
    }

Permissions for custom function queries
---------------------------------------

Permissions configured for the ``SETOF`` table of a function are also applicable to the function itself. E.g. In our text-search example above, if the role ``user`` doesn't have the requisite permissions to view the table ``article``, and the query is run using the ``user`` role, a validation error will be thrown.