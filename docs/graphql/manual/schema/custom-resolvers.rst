
Adding custom GraphQL resolvers
===============================

Hasura GraphQL engine provides instant GraphQL APIs over the tables and views of any Postgres database by
auto-generating the CRUD resolvers. However, sometimes you might have to write custom resolvers to capture business
logic that is unrelated to the database.

We have set up `this boilerplate project <https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/custom-resolvers>`_
illustrating how to write your own custom GraphQL resolvers and merge them with the Hasura GraphQL engine's resolvers.

Follow the boilerplate's ``README.md`` for detailed instructions.

TL;DR
-----
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

The above steps are implemented using `Apollo's graphql-tools library <https://github.com/apollographql/graphql-tools>`__.
