# nodejs-postgres-graphql

> Sample app to demonstrate integration of GraphQL to an existing nodejs backend, with postgres as database, by using the Hasura GraphQL engine over the same postgres database.

[![Edit hasura-to-node-be](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/hasura-to-node-be-nflye?fontsize=14)

**please configure the following in the process environment variables:**
>  `PORT` - to run the process

> `HASURA_SECRET` - `HASURA_GRAPHQL_ADMIN_SECRET` configured in `Settings` of Heroku console

>  `DB_URI` - existing postgres database URI

> `GQL_URI` - GraphQL url from Hasura console


## Running the App

- Run `yarn` to install dependencies
- Run `yarn start` to start the app

### Added Packages

- `apollo-link`, `apollo-link-http` & `apollo-link-error` - GraphQL request handlers
- `graphql-tools` & `node-fetch` - for Schema preparation
- `apollo-server-express` - Apollo Server integration of express

## Tutorial

- Deploy GraphQL Engine on Heroku  using an existing postgres database:
[docs here](https://docs.hasura.io/1.0/graphql/manual/deployment/heroku/using-existing-heroku-database.html)

- Track tables and relationships that needs to be exposed:
[docs here](https://docs.hasura.io/1.0/graphql/manual/schema/using-existing-database.html)

- Grant Postgres Permissions:
[docs here](https://docs.hasura.io/1.0/graphql/manual/deployment/postgres-permissions.html)

- Open the Hasura console:
Click on the `Open app` button in heroku console to open the Hasura app console.

- Copy the `GraphQL Endpoint` and use as `GQL_URI` process environment

- install packages `yarn add apollo-server-express apollo-link apollo-link-http graphql-tools`

- add chainable units of GraphQL request handlers **Apollo Links** in the code ( similar to "middleware" in abstract ):
[apollo-link docs here](https://www.apollographql.com/docs/link/)

- add `graphql-tools` for schema stitching which uses `node-fetch` in code:
[graphql-tools docs here](https://www.apollographql.com/docs/graphql-tools/)

- add Apollo Server integration of `express` which is `apollo-server-express` and create a graphql middleware using the schema and links.

- add graphql middleware to the express app


- Run the app:
>`yarn start`

- Test the app:
Visit [http://localhost:8000/graphql](http://localhost:8000/graphql) to view the graphql playground

# How it works
  It uses [apollo-link](https://www.apollographql.com/docs/link/) and [graphql-tools](https://www.apollographql.com/docs/graphql-tools/) to create a remote schema based on the typedefs and resolvers in the Hasura GraphQL Engine. And creates a GraphQL server with [apollo-server-express](https://github.com/apollographql/apollo-server/tree/master/packages/apollo-server-express)

# Contributing

Checkout the [contributing guide](../../../CONTRIBUTING.md#community-content) for more details.