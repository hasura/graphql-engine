# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine é um servidor GraphQL extremamente rápido que lhe permite criar **instantâneamente, desde APIs GraphQL realtime com Postgres**, até [**gatilhos webhook**](event-triggers.md) em eventos de banco de dados, e [**esquemas remotos**](remote-schemas.md) para lógicas de negócio.

Hashura te ajuda a criar apps com GraphQL e Postgres ou incrementalmente mudar uma aplicação existente que use Postgres.

Leia mais em [hasura.io](https://hasura.io) e as [docs](https://docs.hasura.io).

------------------

![Hasura GraphQL Engine Demo](../assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](../assets/realtime.gif)

-------------------

## Features

* **Faça queries poderosas**: Filtros imbutidos, paginação, pesquisa por patterns, bulk insert, update, delete mutations
* **Tempo real**: Converta qualquer query Graphql para uma query em tempo real usando subscriptions
* **Combine esquemas remotos**: Acesse esquemas GraphQL específcos para a sua lógica de negócio por um único endpoint do GraphQL Engine [**Leia mais**](remote-schemas.md).
* **Ative webhooks e funções serverless**: No Postgres eventos de insert/update/delete  ([leia mais](event-triggers.md))
* **Funciona com bancos de dados em produção**: Aponte o GraphQL Engine para uma instância Postgres existente e obtenha instantâneamente uma API GraphQL pronta para uso.
* **Controle de acesso classe A**: Dynamic access control that integrates with your auth system (eg: auth0, firebase-auth)
* **High-performance & low-footprint**: ~15MB docker image; ~50MB RAM @ 1000 req/s; multi-core aware
* **Admin UI & Migrations**: Admin UI & Rails-inspired schema migrations
* **Postgres** ❤️: Supports Postgres types (PostGIS/geo-location, etc.), turns views to *graphs*, trigger stored functions or procedures with mutations

Leia mais at [hasura.io](https://hasura.io) and the [docs](https://docs.hasura.io).

## Table of contents
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Hasura GraphQL Engine](#hasura-graphql-engine)
  - [Features](#features)
  - [Table of contents](#table-of-contents)
  - [Quickstart:](#quickstart)
    - [One-click deployment on Heroku](#one-click-deployment-on-heroku)
    - [Other one-click deployment options](#other-one-click-deployment-options)
    - [Other deployment methods](#other-deployment-methods)
  - [Architecture](#architecture)
  - [Client-side tooling](#client-side-tooling)
  - [Add business logic](#add-business-logic)
    - [Remote schemas](#remote-schemas)
    - [Trigger webhooks on database events](#trigger-webhooks-on-database-events)
    - [Derived data or data transformations](#derived-data-or-data-transformations)
  - [Demos](#demos)
    - [Realtime applications](#realtime-applications)
    - [Videos](#videos)
  - [Suporte e resolução de problemas](#suporte-e-resolu%c3%a7%c3%a3o-de-problemas)
  - [Contribuindo](#contribuindo)
  - [Recursos da marca](#recursos-da-marca)
  - [Licenças](#licen%c3%a7as)
  - [Traduções](#tradu%c3%a7%c3%b5es)

<!-- markdown-toc end -->

## Quickstart:

### One-click deployment on Heroku

The a maneira mais rápida de usar o Hasura é pelo Heroku: 

1. Click no botão abaixo para fazer deploy do GraphQL Engine no Heroku com o add-on grátis do Postgress:
2. 
    [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

3. Abra o console do Hasura

   Visite `https://<app-name>.herokuapp.com` (*troque \<app-name\> com o nome do seu app*) para abrir o console de administror.

4. Faça sua primeira query GraphQL

   Crie uma table e instantâneamente rode sua primeira query. Siga esse [guia simples](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Other one-click deployment options

Check out the instructions for the following one-click deployment options:

| **Infra provider** |                                                                                                                         **One-click link**                                                                                                                         |                                                            **Additional information**                                                             |
| :----------------: | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: | :-----------------------------------------------------------------------------------------------------------------------------------------------: |
|    DigitalOcean    |                  [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme)                  | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
|       Azure        | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) |                    [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html)                    |

### Other deployment methods

For Docker-based deployment and advanced configuration options, see [deployment
guides](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html) or
[install manifests](install-manifests).

## Architecture

The Hasura GraphQL Engine fronts a Postgres database instance and can accept GraphQL requests from your client apps. It can be configured to work with your existing auth system and can handle access control using field-level rules with dynamic variables from your auth system.

You can also merge remote GraphQL schemas and provide a unified GraphQL API.

![Hasura GraphQL Engine architecture](../assets/hasura-arch.svg)

## Client-side tooling

Hasura works with any GraphQL client. We recommend using [Apollo Client](https://github.com/apollographql/apollo-client). See [awesome-graphql](https://github.com/chentsulin/awesome-graphql) for a list of clients.

## Add business logic

GraphQL Engine provides easy-to-reason, scalable and performant methods for adding custom business logic to your backend:

### Remote schemas

Add custom resolvers in a remote schema in addition to Hasura's Postgres-based GraphQL schema. Ideal for use-cases like implementing a payment API, or querying data that is not in your database - [leia mais](remote-schemas.md).

### Trigger webhooks on database events

Add asynchronous business logic that is triggered based on database events.
Ideal for notifications, data-pipelines from Postgres or asynchronous
processing - [leia mais](event-triggers.md).

### Derived data or data transformations

Transform data in Postgres or run business logic on it to derive another dataset that can be queried using GraphQL Engine - [leia mais](https://docs.hasura.io/1.0/graphql/manual/queries/derived-data.html).

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


## Suporte e resolução de problemas

A documentação e comunidade irão ajudá-lo a solucionar a maioria dos problemas. Se você encontrou um bug ou necessita de entrar em contato conosco, você pode nos contatar usando um dos seguintes canais:

* Suporte e feedback: [Discord](https://discord.gg/vBPpJkS)
* Issue e traqueamento de bugs: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Sigua as atualizações do produto: [@HasuraHQ](https://twitter.com/hasurahq)
* Fale conosco em nosso [chat](https://hasura.io)

Nós estamos empenhados em fomentar um ambiente aberto e acolhedor na comunidade. Por favor, veja o [Código de conduta](code-of-conduct.md).

Se você deseja reportar um problema de segurança, por favor [leia isso](SECURITY.md).

## Contribuindo

Confira o [guia de contribuição](CONTRIBUTING.md) para mais.

## Recursos da marca

Os recursos da marca Hasura (logos, o mascote da Hasura, badges de 'powered by' etc.) podem
ser encontrados na pasta [assets/brand](../assets/brand). Sinta-se livre para usá-los na sua 
aplicação/website etc. Nós ficariamos contentes se você adicionasse o badge "Powered by Hasura"
na sua aplicação feita usando Hasusa. ❤️

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_white.svg" width="150px"/>
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

## Licenças

O núcleo do GraphQL Engine está disponível sob a [Licença Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Todos os **outros conteúdos** (exceto aqueles nos diretórios [`server`](server), [`cli`](cli) e
[`console`](console)) estão disponíveis sob a [Licença MIT](LICENSE-community).
Isso inclui tudo nos diretórios [`docs`](docs) e [`community`](community).

## Traduções

Esse readme está disponível nas seguintes linguagens:

- [English :uk:](.../README.md) (:pray: [@moksahero](https://github.com/moksahero))
- [Japanese :jp:](translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [French :fr:](translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))

Traduções para outros arquivos podem ser encontradas [aqui](translations).
