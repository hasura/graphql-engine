# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)

<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine é um servidor GraphQL extremamento rápido que fornece **instantaneamente, e em tempo real APIs GraphQL no Postgres**, com [**webhook triggers**](event-triggers.md) em eventos de banco de dados, e esquemas remotos para lógica de neócios.

Hasura ajuda a criar GraphQL apps suportados pelo Postgres ou migrar gradualmente para GraphQL apps existentes usando Postgres.

Leia mais em [hasura.io](https://hasura.io) e na [documentação](https://docs.hasura.io).

---

![Hasura GraphQL Engine Demo](../assets/demo.gif)

---

![Hasura GraphQL Engine Realtime Demo](../assets/realtime.gif)

---

## Funcionalidades

- **Faça consultas poderosas**: Built-in filtragem, paginação, pesquisa de padrões, inserção em massa, atualização, exclusão de mutações.
- **Em tempo real**: Converta qualquer consulta GraphQL em uma consulta ativa usando subscriptions.
- **Mesclar esquemas remotos**: Acesse esquemas GraphQL personalizados para lógica de negócios por meio de um único GraphQL Engine endpoint. [Leia mais](remote-schemas.md).
- **Acionar webhooks ou funções sem servidor**: No Postgres insira/atualize/exclua eventos ([leia mais](event-triggers.md))
- **Funciona com bando de dados existentes**: Aponte para um banco de dados Postgres existente para obter uma GraphQL API pronto para uso.
- **Controle de acesso refinado**: Controle de acesso dinamico que se integra ao seu sistema de autenticação(ex: auth0, firebase-auth)
- **Alta performance & low-footprint**: ~15MB docker image; ~50MB RAM @ 1000 req/s; multicore aware
- **Interface de Admin & Migração**: Interface de Admin & esquemas de migração inspirada em Rails
- **Postgres** ❤️: Suporta Postgres types(PostGIS/geo-location, etc.), transforma visualizações em _graphs_, aciona funções ou procedimentos armazenados com mutações

Leia mais em [hasura.io](https://hasura.io) e na [documentação](https://docs.hasura.io).

## Índice

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table des matières**

- [Começo rápido](#quickstart)
  - [Implantação no Heroku em um click](#one-click-deployment-on-heroku)
  - [Outros métodos de implantação](#other-deployment-methods)
- [Arquitetura](#architecture)
- [Ferramentas do client](#client-side-tooling)
- [Adicionar lógica de negócios](#add-business-logic)
  - [Esquemas remotos](#remote-schemas)
  - [Acionar webhooks em eventos de banco de dados](#trigger-webhooks-on-database-events)
- [Demos](#demos)
  - [Aplicações em tempo real](#realtime-applications)
  - [Vídeos](#videos)
- [Suporte & Solução de problemas](#support--troubleshooting)
- [Contribuindo](#contributing)
- [A marca](#brand-assets)
- [Licença](#license)
- [Traduções](#translations)

<!-- markdown-toc end -->

## Começo rápido:

### Implantação no Heroku em um click

O modo mais fácil de experimentar Hasura é via o Heroku.

1. Clique no botão abaixo para implantar GraphQL Engine no Heroku com o complemento gratuito do Postgres:

   [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Abra o console do Hasura

   Visite `https://<app-name>.herokuapp.com` (_substitua \<app-name\> com o nome do seu app_) para abrir o console do admin.

3. Faça sua primeira GraphQL query

   Crie uma tabela e rode instantaneamente sua primeira query. Siga esse [simples guia](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Outros métodos de implantação

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

Hasura GraphQL Engine se place en frontal d'une base de données Postgres et peut accepter les requêtes GraphQL de vos applications clientes. Il peut être configuré pour fonctionner avec votre système d'authentification existant et peut gérer le contrôle d'accès au niveau des champs grâce a des règles, avec des variables dynamiques provenant du système d'authentification.

Vous pouvez également fusionner des schémas GraphQL distants et fournir une API GraphQL unifiée.

![Architecture de Hasura GraphQL Engine](../assets/hasura-arch.svg)

## Outils côté client

Hasura fonctionne avec n'importe quel client GraphQL. Nous recommandons l'utilisation d'[Apollo Client](https://github.com/apollographql/apollo-client). Consultez [awesome-graphql](https://github.com/chentsulin/awesome-graphql) pour une liste de clients.

## Ajout de logique métier

GraphQL Engine fournit plusieurs méthodes claires, évolutives et performantes pour ajouter votre propre logique métier à votre backend:

### Schémas distants

Ajoutez vos propres resolvers dans un schéma distant en plus du schéma dérivé de Postgres d'Hasura. Idéal pour des cas d'utilisation tels que l'implémentation d'une API de paiement, ou le requêtage de donnée ne se trouvant pas dans votre base de données - [plus d'informations](remote-schemas.french.md).

### Déclenchez des webhooks sur des évènements de base de données

Ajoutez de la logique métier asynchrone, déclenchée par des évènements de base de données.
Idéal pour les notifications, les pipelines de données de Postgres ou les
traitements asynchrones - [plus d'informations](event-triggers.french.md).

### Données dérivée ou transformations de données

Transformez les données dans Postgres ou exécutez de la logique métier dessus pour en deriver un autre jeu de données qui peut être requêté à l'aide de GraphQL Engine - [plus d'informations](https://docs.hasura.io/1.0/graphql/manual/queries/derived-data.html).

## Démos

Consultez toutes les applications d'example dans le répertoire
[community/sample-apps](../community/sample-apps).

### Applications temps-réel

- Application de messagerie de groupe développée avec React, incluant un indicateur de frappe, les utilisateurs connectés & les
  notifications de nouveaux messages.

  - [Essayez la](https://realtime-chat.demo.hasura.app/)
  - [Tutoriel](../community/examples/realtime-chat)
  - [Explorez les APIs](https://realtime-chat.demo.hasura.app/console)

- Application de localisation en temps-réel montrant un véhicule dont les coordonnées GPS évoluent
  se déplacer sur une carte.

  - [Essayez la](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutoriel](../community/examples/realtime-location-tracking)
  - [Explorez les APIs](https://realtime-location-tracking.demo.hasura.app/console)

- Un tableau de bord temps-réel pour l'aggrégation de données en constante évolution.
  - [Essayez la](https://realtime-poll.demo.hasura.app/)
  - [Tutoriel](../community/examples/realtime-poll)
  - [Explorez les APIs](https://realtime-poll.demo.hasura.app/console)

### Vidéos

- [Ajoutez GraphQL à une instance GitLab auto-hébergée](https://www.youtube.com/watch?v=a2AhxKqd82Q) (_3:44 mins_)
- [Application de liste de tâches avec Auth0 et un backend GraphQL](https://www.youtube.com/watch?v=15ITBYnccgc) (_4:00 mins_)
- [API GraphQL sur GitLab intégrée avec l'authentification GitLab](https://www.youtube.com/watch?v=m1ChRhRLq7o) (_4:05 mins_)
- [Tableau de bord pour 10 millions de trajets avec géolocalisation (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (_3:06 mins_)

## Support & Dépannage

La documentation et la communauté vous aideront à résoudre la plupart des problèmes. Si vous avez rencontré un bug ou avez besoin de nous contacter, vous pouvez nous joindre au travers des canaux suivants:

- Support & retours: [Discord](https://discord.gg/vBPpJkS)
- Problèmes & remontées de bugs: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
- Suivez les mise à jour du produit: [@HasuraHQ](https://twitter.com/hasurahq)
- Parlez nous sur la [messagerie du site Web](https://hasura.io)

Nous nous impliquons dans le développement d'un environnement ouvert et accueillant dans la communauté. Veuillez consulter le [Code de Conduite](code-of-conduct.french.md).

Si vous souhaitez rapporter un problème de sécurité, veuillez [lire ceci](SECURITY.french.md).

## Contribuer

Consultez notre [guide de contribution](CONTRIBUTING.french.md) pour plus de détails.

## Elements de marque

Les élements de marque Hasura (logos, mascotte Hasura, badges "powered by" etc...) peuvent être
trouvés dans le répertoire [assets/brand](../assets/brand). N'hésitez pas à les utiliser dans votre
application/site Web etc... Nous serions ravis si vous ajoutiez le badge "Powered by Hasura"
à votre application développée à l'aide d'Hasura. ❤️

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_black.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- Pour les fonds clairs -->
<a href="https://hasura.io">
  <img
    width="150px"
    src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_black.svg"
  />
</a>

<!-- Pour les fonds foncés -->
<a href="https://hasura.io">
  <img
    width="150px"
    src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg"
  />
</a>
```

## Licence

Le GraphQL Engine est disponible sous [Apache License
2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Tous les **autres contenus** (à l'exception de ceux dans les répertoires
[`server`](../server), [`cli`](../cli) et [`console`](../console)) sont
disponibles sous [Licence MIT](../LICENSE-community). Cela inclut tout le
contenu des répertoires [`docs`](../docs) et [`community`](../community).
