# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
[![Release](https://img.shields.io/github/release/hasura/graphql-engine/all.svg?style=flat)](https://github.com/hasura/graphql-engine/releases)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)
[![Docker pull](https://img.shields.io/docker/pulls/hasura/graphql-engine.svg?style=flat)](https://hub.docker.com/r/hasura/graphql-engine/)
![Docker image size](https://img.shields.io/microbadger/image-size/hasura/graphql-engine/latest.svg)

[![Chat on Discord](https://img.shields.io/discord/407792526867693568.svg?logo=discord&style=flat)](https://discord.gg/AnmPAz3)
[![Follow on Twitter](https://img.shields.io/twitter/follow/HasuraHQ.svg?style=flat&logo=twitter)](https://twitter.com/intent/follow?screen_name=HasuraHQ)

![Hasura GraphQL Engine Banner](assets/banner.png)

Hasura GraphQL Engine is a performant GraphQL server that provides **ready-to-use GraphQL API over Postgres** by auto-generating GraphQL schema and corresponding resolvers. 

* **Make powerful queries**: built-in filtering, pagination, and pattern search arguments and operators
* **Works with existing database**: point it to an existing, living Postgres database to get instant GraphQL API
* **Fine-grained access control**: dynamic access control that
integrates with your auth
* **Light-weight & performant**: 20MB docker image; ~60MB RAM @ 1000 req/s; multi-core aware
* **Dev friendly**: admin UI & Rails-inspired schema migrations
* **Postgres** ❤️: supports Postgres types (PostGIS/geo-location, etc.), turns views to *graphs*, trigger stored functions or procedures with
mutations

## Quickstart - Setup on Heroku

:information_source: *no credit-card required*; sign-up may be required

1. Click on the following button to deploy GraphQL Engine on Heroku and provision the free Postgres add-on:

    [![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Open the admin console

   Visit https://\<app-name\>.herokuapp.com (*replace \<app-name\> with your app name*) to open the admin console.

3. Make your first GraphQL query

   Create test tables and instantly run your first query. Follow this [simple guide](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Other deployment methods

For Docker-based deployment and advanced configuration options, see deployment guides [here](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html).

## Architecture

The Hasura GraphQL Engine fronts a Postgres database instance and can serve as an API Gateway for all data requests from client apps. It can be configured to work with your existing Auth middleware to handle access control to data using field-level rules with dynamic variables.

The GraphQL Engine is a standalone, multi-core aware component that can be scaled vertically and horizontally.

![Hasura GraphQL Engine architecture](https://hasura.io/rstatic/dist/3021ad7d73fb15e8f7bdf86612ebd8a9.png)

You can also place the engine behind a central GraphQL proxy that fronts multiple GraphQL APIs via schema stitching.

![GraphQL Engine schema-stitched architecture](https://docs.platform.hasura.io/0.15/_images/graphql-schema-stitching.png)

## Client-side tooling

GraphQL Engine provides an HTTP API. This means you can use any client-side tool or library to query the endpoint. To fully leverage GraphQL, it is recommended that you use GraphQL specific client-side tooling like [Apollo Client](https://github.com/apollographql/apollo-client), [Relay](https://github.com/facebook/relay), etc. See [awesome-graphql](https://github.com/chentsulin/awesome-graphql) for a list of such tools.

## Community

GraphQL Engine has a burgeoning community of amazing developers! Come join the community using your preferred means of communication:

- [Discord](https://discord.gg/vBPpJkS)
- [Intercom chat](https://hasura.io/help)
- [Twitter](https://twitter.com/hasurahq)
- [Email](mailto:build@hasura.io)
- [Facebook](https://www.facebook.com/HasuraHQ/)
- [Docs](https://docs.hasura.io)

We are committed to fostering an open and welcoming environment in the community. Please see the [Code of Conduct](code-of-conduct.md) (*adapted from the [Contributor Covenant](https://www.contributor-covenant.org])*).

## Contributing

Checkout our [contributing guide](CONTRIBUTING.md) for more details.

## License

GraphQL Engine is available under the [GNU Affero General Public License v3](https://www.gnu.org/licenses/agpl-3.0.en.html) (AGPL-3.0). [Why AGPL?](https://gist.github.com/hasura-bot/9c36a0201a7563f7762b265a12b044d5).

:information_source: Commercial licenses are available on request. Please feel free to contact us at build@hasura.io or [hasura.io/help](https://hasura.io/help).