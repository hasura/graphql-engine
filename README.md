# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
<a href="https://github.com/hasura/graphql-engine/releases"><img src="https://img.shields.io/badge/release-v1.0.0alpha-brightgreen.svg?style=flat"/></a>
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>

Hasura GraphQL Engine is a blazing-fast GraphQL server that gives you **instant GraphQL APIs over Postgres**. Hasura helps you build GraphQL apps backed by Postgres or incrementally move to GraphQL for existing applications using Postgres.

------------------

_Insert GIF here_

-------------------

* **Make powerful queries**: Built-in filtering, pagination, pattern search, bulk insert, update, delete mutations.
* **Works with existing,live databases**: Point it to an existing Postgres database to instantly get a ready-to-use GraphQL API
* **Fine-grained access control**: Dynamic access control that integrates with your auth system (eg: auth0, firebase-auth)
* **High-performance & low-footprint**: ~15MB docker image; ~50MB RAM @ 1000 req/s; multi-core aware
* **Admin UI & Migrations**: Admin UI & Rails-inspired schema migrations
* **Postgres** ❤️: supports Postgres types (PostGIS/geo-location, etc.), turns views to *graphs*, trigger stored functions or procedures with mutations

Read more at: [https://hasura.io](https://hasura.io) and the [docs](https://docs.hasura.io).

## Demos


* [**Add to Gitlab**](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [**Integrate with Auth0**](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [**Process 10 million rows and use PostGIS for geo-location**](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


| Use-case          | Youtube video |
|:---------------|---------------|
| **Add to Gitlab** | <a href="http://www.youtube.com/watch?feature=player_embedded&v=a2AhxKqd82Q" target="_blank"><img src="https://img.youtube.com/vi/a2AhxKqd82Q/hqdefault.jpg" alt="GraphQL on GitLab" width="240" height="180" /></a> |
| **Integrate with Auth0** | <a href="http://www.youtube.com/watch?feature=player_embedded&v=15ITBYnccgc" target="_blank"><img src="https://img.youtube.com/vi/15ITBYnccgc/hqdefault.jpg" alt="Integrate with Auth0" width="240" height="180" /></a> |
| **Process 10 million rows and use PostGIS for geo-location** | <a href="http://www.youtube.com/watch?feature=player_embedded&v=tsY573yyGWA" target="_blank"><img src="https://img.youtube.com/vi/tsY573yyGWA/hqdefault.jpg" alt="PostGIS with GraphQL" width="240" height="180" /></a> |

## Quickstart - One-click deploy on Heroku

The fastest way to try Hasura out is via Heroku.

1. Click on the following button to deploy GraphQL Engine on Heroku and provision the free Postgres add-on:

    [![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Open the Hasura console

   Visit https://\<app-name\>.herokuapp.com (*replace \<app-name\> with your app name*) to open the admin console.

3. Make your first GraphQL query

   Create a table and instantly run your first query. Follow this [simple guide](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Other deployment methods

For Docker-based deployment and advanced configuration options, see deployment guides [here](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html).

## Architecture

The Hasura GraphQL Engine fronts a Postgres database instance and can accept GraphQL requests from your client apps. It can be configured to work with your existing auth system and can handle access control using field-level rules with dynamic variables from your auth system.

You can also place the engine behind a central GraphQL proxy that fronts multiple GraphQL APIs via schema stitching.

![Hasura GraphQL Engine architecture](assets/hasura-arch.svg)

## Client-side tooling

Hasura works with any GraphQL client. We recommend using [Apollo Client](https://github.com/apollographql/apollo-client). See [awesome-graphql](https://github.com/chentsulin/awesome-graphql) for a list of clients.


## Support & Troubleshooting

The documentation and community will help you troubleshoot most issues. However, if you have encountered a bug or need to get in touch with us, you can contact us using one of the following channels:

* Support & feedaback: [Discord](https://discord.gg/vBPpJkS)
* Issue & bug tracking: [Github issues](https://github.com/hasura/graphql-engine/issues)
* Follow product updates: [@HasuraHQ](https://twitter.com/hasurahq)
* Talk to us on our [website chat](https://hasura.io)

We are committed to fostering an open and welcoming environment in the community. Please see the [Code of Conduct](code-of-conduct.md).

## Contributing

Check out our [contributing guide](CONTRIBUTING.md) for more details.

## License

GraphQL Engine is available under the [GNU Affero General Public License v3](https://www.gnu.org/licenses/agpl-3.0.en.html) (AGPL-3.0), the same license as [MongoDB](https://www.mongodb.com/community/licensing). We have written more about what you can and cannot do under AGPL [here](https://gist.github.com/hasura-bot/9c36a0201a7563f7762b265a12b044d5).

**Commercial licenses are available on request**. Please feel free to contact us at build@hasura.io or on our [website chat](https://hasura.io).
