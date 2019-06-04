Query custom SQL Functions
==========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

What are custom SQL functions?
------------------------------

Custom SQL functions are user-defined SQL functions that can be used to either encapsulate some custom business
logic or extend the built-in SQL functions and operators.

Hasura GraphQL engine lets you expose certain types of custom functions over the GraphQL API to allow querying them
using both ``queries`` and ``subscriptions``.

.. _supported_sql_functions:

Supported SQL functions
-----------------------

Currently, only functions which satisfy the following constraints can be exposed over the GraphQL API
(*terminology from* `Postgres docs <https://www.postgresql.org/docs/current/sql-createfunction.html>`__):

- **Function behaviour**: ONLY ``STABLE`` or ``IMMUTABLE``
- **Return type**: MUST be ``SETOF <table-name>``
- **Argument modes**: ONLY ``IN``

Creating & exposing SQL functions
---------------------------------

Custom SQL functions can be created using SQL which can be run in the Hasura console:

- Head to the ``Data -> SQL`` section of the Hasura console
- Enter your `create function SQL statement <https://www.postgresql.org/docs/current/sql-createfunction.html>`__
- Select the ``Track this`` checkbox to expose the new function over the GraphQL API
- Hit the ``Run`` button

.. note::

  If the ``SETOF`` table doesn't already exist or your function needs to return a custom type i.e. row set,
  create and track an empty table with the required schema to support the function before executing the above
  steps

Querying custom functions using GraphQL queries
-----------------------------------------------

Let's see how we can query custom functions using a GraphQL query by using the below examples:

Example: Text-search functions
******************************

Let's take a look at an example where the ``SETOF`` table is already part of the existing schema.

In our article/author schema, let's say we've created and tracked a custom function, ``search_articles``,
with the following definition:

.. code-block:: plpgsql

  CREATE FUNCTION search_articles(search text)
  RETURNS SETOF article AS $$
      SELECT *
      FROM article
      WHERE
        title ilike ('%' || search || '%')
        OR content ilike ('%' || search || '%')
  $$ LANGUAGE sql STABLE;

This function filters rows from the ``article`` table based on the input text argument, ``search`` i.e. it
returns ``SETOF article``. Assuming the ``article`` table is being tracked, you can use the custom function
as follows:

.. graphiql::
  :view_only:
  :query:
    query {
      search_articles(
        args: {search: "hasura"}
      ){
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

Example: Fuzzy match search functions
*************************************

Let's look at an example of a street address text search with support for misspelled queries.

First install the `pg_trgm <https://www.postgresql.org/docs/current/pgtrgm.html>`__ PostgreSQL extension:

.. code-block:: sql

  CREATE EXTENSION pg_trgm;

Next create a GIN (or GIST) index in your database for the columns you'll be querying:

.. code-block:: sql

  CREATE INDEX address_gin_idx ON property
  USING GIN ((unit || ' ' || num || ' ' || street || ' ' || city || ' ' || region || ' ' || postcode) gin_trgm_ops);

And finally create the custom SQL function in the Hasura console:

.. code-block:: plpgsql

  CREATE FUNCTION search_property(search text)
  RETURNS SETOF property AS $$
      SELECT *
      FROM property
      WHERE
        search <% (unit || ' ' || num || ' ' || street || ' ' || city || ' ' || region || ' ' || postcode)
      ORDER BY
        similarity(search, (unit || ' ' || num || ' ' || street || ' ' || city || ' ' || region || ' ' || postcode)) DESC
      LIMIT 5;
  $$ LANGUAGE sql STABLE;

Assuming the ``property`` table is being tracked, you can use the custom function as follows:

.. graphiql::
  :view_only:
  :query:
    query {
      search_property(
        args: {search: "Unit 2, 25 Foobar St, Sydney NSW 2000"}
      ){
        id
        unit
        num
        street
        city
        region
        postcode
      }
    }
  :response:
    {
      "data": {
        "search_property": [
          {
            "id": 1,
            "unit": "UNIT 2",
            "num": "25",
            "street": "FOOBAR ST",
            "city": "SYDNEY",
            "region": "NSW",
            "postcode": "2000"
          },
          {
            "id": 2,
            "unit": "UNIT 12",
            "num": "25",
            "street": "FOOBAR ST",
            "city": "SYDNEY",
            "region": "NSW",
            "postcode": "2000"
          }
        ]
      }
    }

.. _custom_functions_postgis:

Example: PostGIS functions
**************************

Let's take a look at an example where the ``SETOF`` table is not part of the existing schema.

Say you have 2 tables, for user and landmark location data, with the following definitions (*this example uses the
popular spatial database extension,* `PostGIS <https://postgis.net/>`__):

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

In this example, we want to fetch a list of landmarks that are near a given user, along with the user's details in
the same query. PostGIS' built-in function ``ST_Distance`` can be used to implement this use case.

Since our use case requires an output that isn't a "subset" of any of the existing tables i.e. the ``SETOF`` table
doesn't exist, let's first create this table and then create our location search function.

- create and track the following table:

  .. code-block:: sql

      -- SETOF table
      CREATE TABLE user_landmarks (
        user_id INTEGER,
        location GEOGRAPHY(Point),
        nearby_landmarks JSON
      );

- create and track the following function:

  .. code-block:: plpgsql

      -- function returns a list of landmarks near a user based on the
      -- input arguments distance_kms and userid
      CREATE FUNCTION search_landmarks_near_user(userid integer, distance_kms integer)
      RETURNS SETOF user_landmarks AS $$
        SELECT  A.user_id, A.location,
        (SELECT json_agg(row_to_json(B)) FROM landmark B
         WHERE (
           ST_Distance(
             ST_Transform(B.location::Geometry, 3857),
             ST_Transform(A.location::Geometry, 3857)
           ) /1000) < distance_kms
         ) AS nearby_landmarks
        FROM user_location A where A.user_id = userid
      $$ LANGUAGE sql STABLE;

This function fetches user information (*for the given input* ``userid``) and a list of landmarks which are
less than ``distance_kms`` kilometers away from the user's location as a JSON field. We can now refer to this
function in our GraphQL API as follows:

.. graphiql::
  :view_only:
  :query:
    query {
      search_landmarks_near_user(
        args: {userid: 3, distance_kms: 20}
      ){
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

You can query aggregations on a function result using ``<function-name>_aggregate`` field.

**For example**, count the number of articles returned by the function defined in the text-search example above:

.. code-block:: graphql

      query {
        search_articles_aggregate(
          args: {search: "hasura"}
        ){
          aggregate {
            count
          }
        }
      }

Using arguments with custom functions
*************************************

As with tables, arguments like ``where``, ``limit``, ``order_by``, ``offset``, etc. are also available for use with
function-based queries.

**For example**, limit the number of articles returned by the function defined in the text-search example above:

.. code-block:: graphql

    query {
      search_articles(
        args: {search: "hasura"},
        limit: 5
      ){
        id
        title
        content
      }
    }

Using argument default values for custom functions
**************************************************

If you omit an argument in ``args`` input field then GraphQL Engine executes the SQL function without the argument.
Hence, the function will use the default value of that argument set in its definition.

**For example:** In the above :ref:`PostGIS functions example <custom_functions_postgis>`, the function
definition can be updated as follows:

.. code-block:: plpgsql

      -- input arguments distance_kms (default: 2) and userid
      CREATE FUNCTION search_landmarks_near_user(userid integer, distance_kms integer default 2)

Search nearby landmarks with ``distance_kms`` default value which is 2 kms:

.. graphiql::
  :view_only:
  :query:
    query {
      search_landmarks_near_user(
        args: {userid: 3}
      ){
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
              }
            ]
          }
        ]
      }
    }


Permissions for custom function queries
---------------------------------------

Access control permissions configured for the ``SETOF`` table of a function are also applicable to the function itself.

**For example**, in our text-search example above, if the role ``user`` doesn't have the requisite permissions to view
the table ``article``, a validation error will be thrown if the ``search_articles`` query is run using the ``user``
role.
