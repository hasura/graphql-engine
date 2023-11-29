# Hasura GraphQL Engine

[![Latest release](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
<a href="https://hasura.io/"><img src="assets/brand/hasura_logo_primary_lightbg.svg" align="right" width="200" ></a>
[![Docs](https://img.shields.io/badge/docs-v2.x-brightgreen.svg?style=flat)](https://hasura.io/docs)

<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://hasura.io/newsletter/"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura is an open-source product that accelerates API development by 10x by giving you [GraphQL](https://hasura.io/graphql/) or REST APIs with built-in authorization on your data, instantly.

Read more at [hasura.io](https://hasura.io) and the [docs](https://hasura.io/docs/).

------------------

![Hasura GraphQL Engine Demo](assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](assets/realtime.gif)

-------------------

## Features

* **Make powerful queries**: Built-in filtering, pagination, pattern search, bulk insert, update, delete mutations
* **Works with existing, live databases**: Point it to an existing database to instantly get a ready-to-use GraphQL API
* **Realtime**: Convert any GraphQL query to a live query by using subscriptions
* **Merge remote schemas**: Access custom GraphQL schemas for business logic via a single GraphQL Engine endpoint. [**Read more**](remote-schemas.md).
* **Extend with Actions**: Write REST APIs to extend Hasura’s schema with custom business logic.
* **Trigger webhooks or serverless functions**: On Postgres insert/update/delete events ([read more](event-triggers.md))
* **Scheduled Triggers**: Execute custom business logic at specific points in time using a cron config or a one-off event.
* **Fine-grained access control**: Dynamic access control that integrates with your auth system (eg: auth0, firebase-auth)
* **Admin UI & Migrations**: Admin UI & Rails-inspired schema migrations
* **Supported Databases**: Supports PostgreSQL (and its flavors), MS SQL Server and Big Query. Support for more [databases](https://hasura.io/graphql/database/) coming soon.

Read more at [hasura.io](https://hasura.io) and the [docs](https://hasura.io/docs/).

## Table of contents
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Quickstart:](#quickstart)
    - [One-click deployment on Hasura Cloud](#one-click-deployment-on-hasura-cloud)
    - [Other one-click deployment options](#other-one-click-deployment-options)
    - [Other deployment methods](#other-deployment-methods)
- [Architecture](#architecture)
- [Client-side tooling](#client-side-tooling)
- [Add business logic](#add-business-logic)
    - [Remote schemas](#remote-schemas)
    - [Trigger webhooks on database events](#trigger-webhooks-on-database-events)
- [Demos](#demos)
    - [Realtime applications](#realtime-applications)
    - [Videos](#videos)
- [Support & Troubleshooting](#support--troubleshooting)
- [Stay up to date](#stay-up-to-date)
- [Contributing](#contributing)
- [Brand assets](#brand-assets)
- [License](#license)
- [Translations](#translations)
 
<!-- markdown-toc end -->

## Quickstart:

### One-click deployment on Hasura Cloud

The fastest and easiest way to try Hasura out is via [Hasura Cloud](https://hasura.io/docs/latest/graphql/cloud/getting-started/index.html).

1. Click on the following button to deploy GraphQL engine on Hasura Cloud including Postgres add-on or using an existing Postgres database:

    [![Deploy to Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/signup)

2. Open the Hasura console

   Click on the button "Launch console" to open the Hasura console.

3. Make your first GraphQL query

   Create a table and instantly run your first query. Follow this [simple guide](https://hasura.io/docs/latest/graphql/core/getting-started/first-graphql-query.html).

### Other one-click deployment options

Check out the instructions for the following one-click deployment options:

| **Infra provider** | **One-click link** | **Additional information** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [docs](https://hasura.io/docs/latest/graphql/core/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://hasura.io/docs/latest/graphql/core/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://hasura.io/docs/latest/graphql/core/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [docs](https://hasura.io/docs/latest/graphql/core/guides/deployment/render-one-click.html) |

> Note: The Hasura GraphQL Engine collects anonymous telemetry to understand usage and provide the best experience. Read more [here](https://hasura.io/docs/latest/policies/telemetry/) on what data is collected and the procedure to opt out.

### Other deployment methods

For Docker-based deployment and advanced configuration options, see [deployment
guides](https://hasura.io/docs/latest/graphql/core/getting-started/index.html) or
[install manifests](install-manifests).

## Architecture

The Hasura GraphQL Engine fronts a Postgres database instance and can accept GraphQL requests from your client apps. It can be configured to work with your existing auth system and can handle access control using field-level rules with dynamic variables from your auth system.

You can also merge remote GraphQL schemas and provide a unified GraphQL API.

![Hasura GraphQL Engine architecture](assets/hasura-arch.svg)

## Client-side tooling

Hasura works with any GraphQL client. See [awesome-graphql](https://github.com/chentsulin/awesome-graphql) for a list of clients. Our [frontend tutorial series](https://hasura.io/learn/#frontend-tutorial) also have integrations with GraphQL clients for different frameworks.

## Add business logic

GraphQL Engine provides easy-to-reason, scalable and performant methods for adding custom business logic to your backend:

### Remote schemas

Add custom resolvers in a remote schema in addition to Hasura's database-based GraphQL schema. Ideal for use-cases like implementing a payment API, or querying data that is not in your database - [read more](remote-schemas.md).

### Actions

Actions are a way to extend Hasura’s schema with custom business logic using custom queries and mutations. Actions can be added to Hasura to handle various use cases such as data validation, data enrichment from external sources and any other complex business logic - [read more](https://hasura.io/docs/latest/graphql/core/actions/index.html)

### Trigger webhooks on database events

Add asynchronous business logic that is triggered based on database events.
Ideal for notifications, data-pipelines from Postgres or asynchronous
processing - [read more](event-triggers.md).

### Derived data or data transformations

Transform data in Postgres or run business logic on it to derive another dataset that can be queried using GraphQL Engine - [read more](https://hasura.io/docs/latest/graphql/core/queries/derived-data.html).

## Demos

Check out all the example applications in the [hasura/sample-apps](https://github.com/hasura/sample-apps/tree/main) repository.

### Realtime applications

- Group Chat application built with React, includes a typing indicator, online users & new
  message notifications.
  - [Try it out](https://realtime-chat.demo.hasura.io/)
  - [Tutorial](https://github.com/hasura/sample-apps/tree/main/realtime-chat)

- Live location tracking app that shows a running vehicle changing the current GPS
  coordinates moving on a map.
  - [Try it out](https://realtime-location-tracking.demo.hasura.io/)
  - [Tutorial](https://github.com/hasura/sample-apps/tree/main/realtime-location-tracking)

- A real-time dashboard for data aggregations on continuously changing data.
  - [Try it out](https://realtime-poll.demo.hasura.io/)
  - [Tutorial](https://github.com/hasura/sample-apps/tree/main/realtime-poll)

### Videos

* [Add GraphQL to a self-hosted GitLab instance](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Todo app with Auth0 and GraphQL backend](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GraphQL on GitLab integrated with GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [Dashboard for 10million rides with geo-location (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)

## Support & Troubleshooting

The documentation and community will help you troubleshoot most issues. If you have encountered a bug or need to get in touch with us, you can contact us using one of the following channels:

* Support & feedback: [Discord](https://discord.gg/hasura)
* Issue & bug tracking: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Follow product updates: [@HasuraHQ](https://twitter.com/hasurahq)
* Talk to us on our [website chat](https://hasura.io)

We are committed to fostering an open and welcoming environment in the community. Please see the [Code of Conduct](code-of-conduct.md).

If you want to report a security issue, please [read this](SECURITY.md).

## Stay up to date

We release new features every month. Sign up for our newsletter by using the link below. We send newsletters only once a month.
[https://hasura.io/newsletter/](https://hasura.io/newsletter/)

## Contributing

Check out our [contributing guide](CONTRIBUTING.md) for more details.

## Brand assets

Hasura brand assets (logos, the Hasura mascot, powered by badges etc.) can be
found in the [assets/brand](assets/brand) folder. Feel free to use them in your
application/website etc. We'd be thrilled if you add the "Powered by Hasura"
badge to your applications built using Hasura. ❤️

<div style="display: flex;">
  <img src="assets/brand/powered_by_hasura_primary_darkbg.svg" width="150px"/>
  <img src="assets/brand/powered_by_hasura_primary_lightbg.svg" width="150px"/>
</div>

```html
<!-- For light backgrounds -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_primary_darkbg.svg" />
</a>

<!-- For dark backgrounds -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_primary_lightbg.svg" />
</a>
```

## License

The core GraphQL Engine is available under the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

All **other contents** (except those in [`server`](server), [`cli`](cli) and
[`console`](console) directories) are available under the [MIT License](LICENSE-community).
This includes everything in the [`docs`](docs) and [`community`](community)
directories.

## Translations

This readme is available in the following translations:

- [Japanese :jp:](translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [French :fr:](translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Bosnian :bosnia_herzegovina:](translations/README.bosnian.md) (:pray: [@hajro92](https://github.com/hajro92))
- [Russian :ru:](translations/README.russian.md) (:pray: [@highflyer910](https://github.com/highflyer910))
- [Greek 🇬🇷](translations/README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))
- [Spanish 🇲🇽](/translations/README.mx_spanish.md)(:pray: [@ferdox2](https://github.com/ferdox2))
- [Indonesian :indonesia:](translations/README.indonesian.md) (:pray: [@anwari666](https://github.com/anwari666))
- [Brazilian Portuguese :brazil:](translations/README.portuguese_br.md) (:pray: [@rubensmp](https://github.com/rubensmp))
- [German 🇩🇪](translations/README.german.md) (:pray: [@FynnGrandke](https://github.com/FynnGrandke))
- [Chinese :cn:](translations/README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))
- [Turkish :tr:](translations/README.turkish.md) (:pray: [@berat](https://github.com/berat))
- [Korean :kr:](translations/README.korean.md) (:pray: [@라스크](https://github.com/laskdjlaskdj12))
- [Italian :it:](translations/README.italian.md) (:pray: [@befire](https://github.com/francesca-belfiore))

Translations for other files can be found [here](translations).
