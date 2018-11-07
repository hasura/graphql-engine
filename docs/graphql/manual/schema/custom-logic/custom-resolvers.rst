Custom GraphQL resolvers
========================

Custom GraphQL resolvers can be used for:

- Adding additional fields to the top-level fields exposed in the Hasura GraphQL engine schema
- Adding relationships between fields from GraphQL Engine's schema and those from a different GraphQL schema (in conjunction with schema stitching)
- Add a layer of business logic like data validation, etc. before inserting data into the database

Custom resolvers can be added to the Hasura GraphQL schema using the `Apollo's graphql-tools library <Apollo_>`__. At a high level, you need to:

#. Define custom fields that extend GraphQL Engine's types (new fields or modify existing ones).
#. Merge the different schemas i.e. GraphQL Engine's schema and the schema from the above step (and any other GraphQL schemas needed).
#. Support the new fields by writing custom resolvers that handle the required logic or delegate to one of the stitched GraphQL schemas.

.. note::

  Adding an additional layer on top of Hasura GraphQL engine significantly impacts the performance provided by it out of the box (*by as much as 4X*). Hence, if you don't need to make changes to the fields exposed by the Hasura GraphQL engine, see :doc:`schema-stitching` for a more performant solution

**Examples**

Let's explore two use cases to understand how a combination of schema stitching and resolvers is used to modifying the GraphQL schema fields exposed by Hasura GraphQL engine.

Extend GraphQL Engine's schema with related data
------------------------------------------------

Assume the following database schema in PostgreSQL:

+----------------------------------------+----------------------------------------+
|Table                                   |Columns                                 |
+========================================+========================================+
|person                                  |id, name, city                          |
+----------------------------------------+----------------------------------------+

We have a simple ``person`` table with columns ``id``, ``name`` and ``city``. For this example, the above table has
anonymous select permission.

The GraphQL query in Hasura Data API for the above table would look like:

.. code-block:: graphql

    query fetch_person {
      person {
        id
        name
        city
      }
    }

This is a simple select on table person.

On the other hand, we have a GraphQL server for fetching weather information that connects to ``Meta Weather API``.

The GraphQL schema for this weather API looks like:

.. code-block:: graphql

    type CityWeather {
      temp: String
      min_temp: String
      max_temp: String
      city_name: String!
      applicable_date: String!
    }

.. code-block:: graphql

    type Query {
      cityWeather(city_name: String! applicable_date: String): CityWeather
    }

The GraphQL query to fetch this weather information would look like:

.. code-block:: graphql

    query {
      cityWeather (city_name: "Bangalore") {
        city_name
        temp
        min_temp
        max_temp
        applicable_date
      }
    }

Explore this API on `Apollo LaunchPad <https://launchpad.graphql.com/nxw8w0z9q7>`_.

Note the usage of ``city_name`` as an argument for the ``cityWeather`` query. Using this we can extend our original Postgres's ``person`` schema to include weather information based on the ``city`` column of the person table.

.. code-block:: graphql

    extend type person {
      city_weather: CityWeather,
    }

We have extended the type person to have one more field called ``city_weather``. This will resolve to the weather schema defined above and the respective resolver will return appropriate data.

The source code for the custom resolver can be found on GitHub - `graphql-schema-stitching-demo
<https://github.com/hasura/graphql-schema-stitching-demo>`_. Note the usage of ``mergeSchemas``, a ``graphql-tools`` utility that enables schema stitching.

Now the merged schema can be queried as:

.. code-block:: graphql

    query {
      person {
        id
        name
        city
        city_weather {
          city_name
          temp
          min_temp
          max_temp
          applicable_date
        }
      }
    }

This is a neat abstraction for the client making the GraphQL API, as all the merging of different schemas are done by the server and exposed as a single API.

Add new fields to GraphQL Engine's schema
-----------------------------------------
We have set up `this boilerplate project <https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/custom-resolvers>`_
that illustrates how to add new fields to GraphQL Engine's schema.

Follow the boilerplate's ``README.md`` for detailed instructions.

TL;DR
^^^^^
The boilerplate includes sample custom resolvers for:

- A ``hello`` query which returns a fixed string.
- A ``count`` query that returns a counter from some other data source.
- A ``increment_counter`` mutation that increments the value of the above counter.
- A ``user_average_age`` query that directly makes an SQL query to Postgres using Knex and returns the result.

The steps to achieve this are:

- Create the query/mutation types for your custom GraphQL API.
- Write the custom resolver code for the above types.
- Make a new GraphQL schema out of these custom resolvers.
- Merge this schema with the existing Hasura GraphQL schema and serve the resulting GraphQL API.

.. _Apollo: https://github.com/apollographql/graphql-tools