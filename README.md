# Hasura GraphQL Engine

[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=svg)](https://circleci.com/gh/hasura/graphql-engine)

Hasura GraphQL Engine is a performant GraphQL server that provides a **ready-to-use GraphQL API over Postgres** by auto-generating a GraphQL schema and corresponding resolvers. 

* Make powerful queries: built-in filtering, pagination and pattern search arguments and operators
* ??: add it to an existing, living Postgres database
* Fine-grained access control: dynamic access control that
integrates with your auth
* Lightweight & blazing fast: stats + link to Performance Benchmarks section
* Dev friendly: Admin UI & Rails-inspired schema migrations
* Postgres ❤️: supports Postgres types (PostGIS/geo-location, etc.), turns views to *graphs*, trigger stored functions or procedures with
mutations

## Quickstart - Setup on Heroku

:information_source: *no credit-card required*; Sign-up may be required

1. Click on the following button to deploy GraphQL Engine on Heroku and provision the free Postgres add-on:

    [![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Open the admin console

   Visit https://\<app-name\>.herokuapp.com (replace \<app-name\> with your app name) to open the admin console.

3. Make your first GraphQL query

   Create test tables and instantly run your first query. Follow this [simple guide] (https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Other deployment methods

See deployment guides [here](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html) for Docker-based deployment and advanced configuration options.

## Architecture

The Hasura GraphQL Engine fronts a Postgres database instance and can serve as an API Gateway for all data requests from client apps. It can be configured to work with your existing Auth middleware to handle access control to data using field-level rules with dynamic variables.

The GraphQL Engine is a standalone, multi-core aware component that can be scaled vertically and horizontally.**??**

![alt text](https://hasura.io/rstatic/dist/3021ad7d73fb15e8f7bdf86612ebd8a9.png "GraphQL Engine basic architecture")

You can also place the engine behind a central GraphQL proxy that fronts multiple GraphQL APIs via schema stitching.

![alt text](https://docs.platform.hasura.io/0.15/_images/graphql-schema-stitching.png "GraphQL Engine schema-stitched architecture")

## Client-side tooling

GraphQL Engine provides an http API. This means you can use any client-side tool or library to query the endpoint. To fully leverage GraphQL, it is recommended that you use GraphQL specific client-side tooling like [Apollo Client](https://github.com/apollographql/apollo-client), [Relay](https://github.com/facebook/relay), etc. See [awesome-graphql](https://github.com/chentsulin/awesome-graphql) for a list of such tools.

## Community

GraphQL Engine has a burgeoning community of amazing developers! Come join the community using your preferred means of communication:

- [Discord](https://discord.gg/vBPpJkS)
- [Intercom chat](https://hasura.io/help)
- [Twitter](https://twitter.com/hasurahq)
- [Email](mailto:build@hasura.io)
- [Facebook](https://www.facebook.com/HasuraHQ/)
- [Docs](https://docs.hasura.io)

Hasura follows the CNCF code of conduct.

## Contributing

Checkout our [contributing guide](CONTRIBUTING.md) for more details.

## License

GraphQL Engine is available under the [GNU Affero General Public License v3](https://www.gnu.org/licenses/agpl-3.0.en.html) (AGPL-3.0). [Why AGPL](https://gist.github.com/hasura-bot/9c36a0201a7563f7762b265a12b044d5).

:information_source: If you are not satisfied with this license, commercial licenses are available on request. Please feel free to contact us at build@hasura.io or [hasura.io/help](https://hasura.io/help).
