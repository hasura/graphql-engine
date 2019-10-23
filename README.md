# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine é um servidor extremamente rápido que oferece a você criar **instantâneamente, em tempo real, APIs GraphQL baseadas no Postgres**, com [**webhook triggers**](event-triggers.md) em eventos do banco de dados, e [**remote schemas**](remote-schemas.md) para a regra de negócio.

Hasura te ajuda a contruir aplicativos GraphQL suportados pelo Postgres ou migrar uma aplicação já existente, que utiliza Postgres, para GraphQL.

Leia mais em [hasura.io](https://hasura.io) e na [documentação](https://docs.hasura.io).

------------------

![Hasura GraphQL Engine Demo](assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](assets/realtime.gif)

-------------------

## Funcionalidades

* **Realize queries avançadas**: filtragem, paginação, busca de padrões, inserção em massa, atualizações, exclusões
* **Em Tempo-Real**: Converta qualquer consulta do GraphQL em uma consulta em tempo real usando as assinaturas.
* **Faça o Merge de Schemas remotos**: Acesse seus próprios esquemas GraphQL para sua lógica de negócios através de um único ponto de acesso GraphQL. [**Leia Mais**](remote-schemas.md).
* **Disparar webhooks ou funções sem servidor**: Em resposta aos eventos do Postgres, insira / atualize / exclua ([leia mais](event-triggers.md))
* **Funciona com banco de dados já existentes**: direcione o GraphQL Engine para um banco de dados Postgres existente para obter instantaneamente uma API GraphQL pronta para uso.
* **Controle de acesso detalhado**: Controle de acesso dinâmico que integra com o seu sistema de autorização (ex: auth0, firebase-auth)
* **Alto desempenho e baixo impacto**: ~15MB de imagem do Docker; ~50MB RAM @ 1000 req/s; levando em conta o multi-core
* **Interface de administração e migração**: Interface de administração e migração de schemas inspirados no Rails
* **Postgres** ❤️: suporta tipos do Postgres (PostGIS / localização geográfica, etc.), transforma visualizações em gráficos, aciona procedimentos ou funções armazenados através de mutações

Leia mais em [hasura.io](https://hasura.io) e a [documentação](https://docs.hasura.io).

## Conteúdo
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Conteúdo**

- [Primeiros passos:](#quickstart)
    - [Deploy no Heroku em um clique](#one-click-deployment-on-heroku)
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
- [Contributing](#contributing)
- [Brand assets](#brand-assets)
- [License](#license)
- [Translations](#translations)

<!-- markdown-toc end -->

## Primeiros passos:

### Deploy no Heroku em um clique

A maneira mais rápida de experimentar o Hasura é via Heroku.

1. Click on the following button to deploy GraphQL Engine on Heroku with the free Postgres add-on:

    [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Open the Hasura console

   Visit `https://<app-name>.herokuapp.com` (*replace \<app-name\> with your app name*) to open the admin console.

3. Make your first GraphQL query

   Create a table and instantly run your first query. Follow this [simple guide](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Other one-click deployment options

Check out the instructions for the following one-click deployment options:

| **Infra provider** | **One-click link** | **Additional information** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |

### Other deployment methods

For Docker-based deployment and advanced configuration options, see [deployment
guides](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html) or
[install manifests](install-manifests).

## Architecture

The Hasura GraphQL Engine fronts a Postgres database instance and can accept GraphQL requests from your client apps. It can be configured to work with your existing auth system and can handle access control using field-level rules with dynamic variables from your auth system.

You can also merge remote GraphQL schemas and provide a unified GraphQL API.

![Hasura GraphQL Engine architecture](assets/hasura-arch.svg)

## Client-side tooling

Hasura works with any GraphQL client. We recommend using [Apollo Client](https://github.com/apollographql/apollo-client). See [awesome-graphql](https://github.com/chentsulin/awesome-graphql) for a list of clients.

## Add business logic

GraphQL Engine provides easy-to-reason, scalable and performant methods for adding custom business logic to your backend:

### Remote schemas

Add custom resolvers in a remote schema in addition to Hasura's Postgres-based GraphQL schema. Ideal for use-cases like implementing a payment API, or querying data that is not in your database - [read more](remote-schemas.md).

### Trigger webhooks on database events

Add asynchronous business logic that is triggered based on database events.
Ideal for notifications, data-pipelines from Postgres or asynchronous
processing - [read more](event-triggers.md).

### Derived data or data transformations

Transform data in Postgres or run business logic on it to derive another dataset that can be queried using GraphQL Engine - [read more](https://docs.hasura.io/1.0/graphql/manual/queries/derived-data.html).

## Demos

Check out all the example applications in the
[community/examples](community/examples) directory.

### Realtime applications

- Group Chat application built with React, includes a typing indicator, online users & new
  message notifications.
  - [Try it out](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-chat)
  - [Browse APIs](https://realtime-chat.demo.hasura.app/console)

- Live location tracking app that shows a running vehicle changing current GPS
  coordinates moving on a map.
  - [Try it out](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-location-tracking)
  - [Browse APIs](https://realtime-location-tracking.demo.hasura.app/console)

- A realtime dashboard for data aggregations on continuously changing data.
  - [Try it out](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-poll)
  - [Browse APIs](https://realtime-poll.demo.hasura.app/console)

### Videos

* [Add GraphQL to a self-hosted GitLab instance](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Todo app with Auth0 and GraphQL backend](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GraphQL on GitLab integrated with GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [Dashboard for 10million rides with geo-location (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


## Support & Troubleshooting

The documentation and community will help you troubleshoot most issues. If you have encountered a bug or need to get in touch with us, you can contact us using one of the following channels:

* Support & feedback: [Discord](https://discord.gg/vBPpJkS)
* Issue & bug tracking: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Follow product updates: [@HasuraHQ](https://twitter.com/hasurahq)
* Talk to us on our [website chat](https://hasura.io)

We are committed to fostering an open and welcoming environment in the community. Please see the [Code of Conduct](code-of-conduct.md).

If you want to report a security issue, please [read this](SECURITY.md).

## Contributing

Check out our [contributing guide](CONTRIBUTING.md) for more details.

## Brand assets

Hasura brand assets (logos, the Hasura mascot, powered by badges etc.) can be
found in the [assets/brand](assets/brand) folder. Feel free to use them in your
application/website etc. We'd be thrilled if you add the "Powered by Hasura"
badge to your applications built using Hasura. ❤️

<div style="display: flex;">
  <img src="assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- For light backgrounds -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- For dark backgrounds -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
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
- [Greek 🇬🇷](translations/README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))

Translations for other files can be found [here](translations).
