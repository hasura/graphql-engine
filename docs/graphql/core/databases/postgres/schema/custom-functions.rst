.. meta::
   :description: Customise the Hasura GraphQL schema with Postgres SQL functions
   :keywords: hasura, docs, postgres, schema, sql functions, stored procedures

.. _custom_sql_functions:

Postgres: Extend schema with SQL functions
==========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

What are custom SQL functions?
------------------------------

Custom SQL functions are `user-defined SQL functions <https://www.postgresql.org/docs/current/sql-createfunction.html>`__
that can be used to either encapsulate some custom business logic or extend the built-in SQL functions and operators. SQL functions
are also referred to as **stored procedures**.

Hasura GraphQL engine lets you expose certain types of custom functions as top level fields in the GraphQL API to allow
querying them with either ``queries`` or ``subscriptions``, or for ``VOLATILE`` functions as ``mutations``.

.. note::

  Custom SQL functions can also be queried as :ref:`computed fields <computed_fields>` of tables.

.. _supported_sql_functions:

Supported SQL functions
***********************

Currently, only functions which satisfy the following constraints can be exposed as top level fields in the GraphQL API
(*terminology from* `Postgres docs <https://www.postgresql.org/docs/current/sql-createfunction.html>`__):

- **Function behaviour**: ``STABLE`` or ``IMMUTABLE`` functions may *only* be exposed as queries. 
  ``VOLATILE`` functions may be exposed as mutations or queries. 
- **Return type**: MUST be ``SETOF <table-name>`` OR ``<table-name>`` where ``<table-name>`` is already tracked
- **Argument modes**: ONLY ``IN``

.. _create_sql_functions:

Creating SQL functions
----------------------

SQL functions can be created using SQL statements which can be executed as follows:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    - Head to the ``Data -> SQL`` section of the Hasura console
    - Enter your `create function SQL statement <https://www.postgresql.org/docs/current/sql-createfunction.html>`__
    - Hit the ``Run`` button

  .. tab:: CLI

    1. :ref:`Create a migration manually <manual_migrations>` and add your `create function SQL statement <https://www.postgresql.org/docs/current/sql-createfunction.html>`__ to the ``up.sql`` file. Also, add an SQL statement that reverts the previous statement to the ``down.sql`` file in case you need to :ref:`roll back <roll_back_migrations>` the migrations.

    2. Apply the migration by running:

       .. code-block:: bash

         hasura migrate apply

  .. tab:: API

    You can add a function by making an API call to the :ref:`run_sql metadata API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "<create function statement>"
        }
      }

.. _track_custom_sql_functions:

Track SQL functions
-------------------

Functions can be present in the underlying Postgres database without being exposed over the GraphQL API.
In order to expose a function over the GraphQL API, it needs to be **tracked**.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    While creating functions from the ``Data -> SQL`` page, selecting the ``Track this`` checkbox
    will expose the new function over the GraphQL API right after creation if it is supported.

    You can track any existing supported functions in your database from the ``Data -> Schema`` page:

    .. thumbnail:: /img/graphql/core/schema/schema-track-functions.png
      :alt: Track functions

  .. tab:: CLI

    1. To track the function and expose it over the GraphQL API, edit the ``functions.yaml`` file in the ``metadata`` directory as follows:

       .. code-block:: yaml
         :emphasize-lines: 1-3

          - function:
              schema: public
              name: <function name>

    2. Apply the metadata by running:

       .. code-block:: bash

         hasura metadata apply

  .. tab:: API

    To track the function and expose it over the GraphQL API, make the following API call to the :ref:`track_function metadata API <track_function>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "track_function",
        "args": {
          "schema": "public",
          "name": "<name of function>"
        }
      }

.. note::

  If the ``SETOF`` table doesn't already exist or your function needs to return a custom type i.e. row set,
  create and track an empty table with the required schema to support the function before executing the above
  steps.

Use cases
---------

Custom functions are ideal solutions for retrieving some derived data based on some custom business logic that
requires user input to be calculated. If your custom logic does not require any user input, you can use
:ref:`views <custom_views>` instead.

Let's see a few example use cases for custom functions:

Example: Text-search functions
******************************

Let's take a look at an example where the ``SETOF`` table is already part of the existing schema:

.. code-block:: plpgsql

  articles(id integer, title text, content text)

Let's say we've created and tracked a custom function, ``search_articles``, with the following definition:

.. code-block:: plpgsql

  CREATE FUNCTION search_articles(search text)
  RETURNS SETOF articles AS $$
      SELECT *
      FROM articles
      WHERE
        title ilike ('%' || search || '%')
        OR content ilike ('%' || search || '%')
  $$ LANGUAGE sql STABLE;

This function filters rows from the ``articles`` table based on the input text argument, ``search`` i.e. it
returns ``SETOF articles``. Assuming the ``articles`` table is being tracked, you can use the custom function
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

  CREATE INDEX address_gin_idx ON properties
  USING GIN ((unit || ' ' || num || ' ' || street || ' ' || city || ' ' || region || ' ' || postcode) gin_trgm_ops);

And finally create the custom SQL function in the Hasura console:

.. code-block:: plpgsql

  CREATE FUNCTION search_properties(search text)
  RETURNS SETOF properties AS $$
      SELECT *
      FROM properties
      WHERE
        search <% (unit || ' ' || num || ' ' || street || ' ' || city || ' ' || region || ' ' || postcode)
      ORDER BY
        similarity(search, (unit || ' ' || num || ' ' || street || ' ' || city || ' ' || region || ' ' || postcode)) DESC
      LIMIT 5;
  $$ LANGUAGE sql STABLE;

Assuming the ``properties`` table is being tracked, you can use the custom function as follows:

.. graphiql::
  :view_only:
  :query:
    query {
      search_properties(
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
        "search_properties": [
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

Querying custom functions using GraphQL queries
-----------------------------------------------

Aggregations on custom functions
********************************

You can query aggregations on a function result using the ``<function-name>_aggregate`` field.

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

If you omit an argument in the ``args`` input field then the GraphQL engine executes the SQL function without the argument.
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


Accessing Hasura session variables in custom functions
******************************************************

Create a function with an argument for session variables and track it with the :ref:`track_function_v2 <track_function_v2>` API with the
``session_argument`` config set. The session argument will be a JSON object where keys are session variable names (in lower case)
and values are strings. Use the ``->>`` JSON operator to fetch the value of a session variable as shown in the
following example.

.. code-block:: plpgsql

      -- single text column table
      CREATE TABLE text_result(
        result text
      );

      -- simple function which returns the hasura role
      -- where 'hasura_session' will be session argument
      CREATE FUNCTION get_session_role(hasura_session json)
      RETURNS SETOF text_result AS $$
          SELECT q.* FROM (VALUES (hasura_session ->> 'x-hasura-role')) q
      $$ LANGUAGE sql STABLE;


.. graphiql::
  :view_only:
  :query:
     query {
       get_session_role {
         result
       }
     }
  :response:
    {
      "data": {
        "get_session_role": [
          {
            "result": "admin"
          }
        ]
      }
    }

.. note::

   The specified session argument will not be included in the ``<function-name>_args`` input object in the GraphQL schema.


Tracking functions with side effects
************************************

You can also use the :ref:`track_function_v2 <track_function_v2>` API to track
`VOLATILE functions <https://www.postgresql.org/docs/current/xfunc-volatility.html>`__
as mutations.

Aside from showing up under the ``mutation`` root (and presumably having
side-effects), these tracked functions behave the same as described above for
``queries``.

We also permit tracking ``VOLATILE`` functions under the ``query`` root, in
which case the user needs to guarantee that the field is idempotent and
side-effect free, in the context of the resulting GraphQL API. One such use
case might be a function that wraps a simple query and performs some logging
visible only to administrators.

.. note::

   It's easy to accidentally give an SQL function the wrong volatility (or for a
   function to end up with ``VOLATILE`` mistakenly, since it's the default).


Permissions for custom functions
--------------------------------

A custom function ``f`` is only accessible to a role ``r`` if there is a function permission (see :ref:`Create function permission <pg_create_function_permission>`) defined on the function ``f`` for the role ``r``. Additionally, role ``r`` must have SELECT permissions on the returning table of the function ``f``.

:ref:`Access control permissions <permission_rules>` configured for the ``SETOF`` table of a function are also applicable to the function itself.

.. TODO: add setting fn permission section (API, CLI, Console)

**For example**, in our text-search example above, if the role ``user`` has access only to certain columns of
the table ``article``, a validation error will be thrown if the ``search_articles`` query is run selecting a column
to which the ``user`` role doesn't have access to.

.. note::

   In case of **functions exposed as queries**, if the Hasura GraphQL engine is started with inferring of function permissions
   set to ``true`` (by default: ``true``) then a function exposed as a query will be accessible to a role even
   if the role doesn't have a function permission for the function - provided the role has select permission defined
   on the returning table of the function.
