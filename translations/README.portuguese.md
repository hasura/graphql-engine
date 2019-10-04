# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine é um servidor GraphQL ultra rápido que lhe proporciona **instantaneamente e em tempo real uma API GraphQL para Postgres**, contendo [**gatilhos webhooks**](event-triggers.md) em eventos de bancos de dados, juntamente com [**esquemas remotos**](remote-schemas.md) para lógica de negócio.

Hasura ajuda na construção aplicações GraphQL que utilizam Postgres ou incrementalmente mmigre para o GraphQL para aplicações já existentes e que utilizam Postgres.

Leia mais em [hasura.io](https://hasura.io) e nos [docs](https://docs.hasura.io).

------------------

![Hasura GraphQL Engine Demo](assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](assets/realtime.gif)

-------------------

## Funcionalidades

* **Faça queries poderosas**: Filtros imbutidos, paginação, padrões de busca, inserções em massa, atualize, delete modificações;
* **Tempo real**: Converta qualquer query GraphQL para uma _live query_ (query em tempo real) utilizando _subscriptions_ (assinaturas);
* **Merges em esquemas remotos**: Acesse esquemas GraphQL personalizados para lógica de negócio via endpoint do GraphQL Engine [**Saiba mais**](remote-schemas.md);
* **Gatilhos webhooks or funções serverless**: em eventos _insert/update/delete_ do Postgres ([leia mais](event-triggers.md));
* **Funciona com bancos de dados existentes, em tempo reals**: Aponte para um banco de dados Postgres existente para ter uma API GraphQL instantaneamente e pronta para uso;
* **Controle de acesso fino e granular**: Controle de acesso dinâmico que integra com seu sistema de autenticação (ex: auth0, firebase-auth);
* ** Alta performance & Baixo consumo de hardware**: aproximadamente 15MB no docker image; aprocimadamente 50MB RAM @ 1000 requisições/s; serviços multi-core (multi-core aware);
* **Interface de Usuário _Admin_ & Migrações**: Interfaqce de usuário _Admin_ e migrações de esquemas inspiradas em Rails;
* **Postgres** ❤️: Suporte aos Postgres _types_ (PostGIS/geo-location, etc.), transforma _views_ em *graphs*, funções de gatilhos armazenados ou _procedures_ com mutações.

Leia mais em [hasura.io](https://hasura.io) e nos [docs](https://docs.hasura.io).

## Sumário
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Sumário**

- [Quickstart:](#quickstart)
    - [Deploy em um clique no Heroku](#one-click-deployment-on-heroku)
    - [Outros métodos de deploy](#other-deployment-methods)
- [Arquitetura](#architecture)
- [Ferramental _Client-side_](#client-side-tooling)
- [Adicionando lógica de negócio](#add-business-logic)
    - [Esquemas remotos](#remote-schemas)
    - [Gatilhos _webhook_ em eventos de bancos de dados](#trigger-webhooks-on-database-events)
- [Demos](#demos)
    - [Aplicações em tempo real](#realtime-applications)
    - [Vídeos](#videos)
- [Suporte & Troubleshooting](#support--troubleshooting)
- [Contribuindo](#contributing)
- [Ativos de marketing](#brand-assets)
- [Licença](#license)
- [Traduções](#translations)

<!-- markdown-toc end -->

## Quickstart:

### _Deploy_ em um clique no Heroku

A maneira mais rápida de utilizar o Hasura é via Heroku.

1. Clique no botão a seguir para fazer o _deploy_ do GraphQL Engine no Heroku com o add-on gratuito do Postgres:

    [![_Deploy_ no Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Abra o console Hasura

   Visite `https://<app-name>.herokuapp.com` (*substitua \<app-name\> pelo nome de sua aplicação*)  para abrir o console de administrador.

3. Faça sua primeira query GraphQL

   Crie uma tabela e rode sua primeira _query_ instantaneamente. Siga esse [guia simples](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Outras opções de _deploy_ em um clique

Confira as instruções para as opções de _deploy_ em um clique a seguir:

| **Infra provider** | **One-click link** | **Additional information** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |

### Outros métodos de _deploy_

Para _deploys_ baseados em Docker e opções avançadas de configuração, veja [guias de _deploy_]
(https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html) ou
[manifestos de instalação](install-manifests).

## Arquitetura

O Engine Hasura GraphQL faz o fronte de uma instância de banco de dados Postgres e pode aceitar requisições GraphQL
de suas aplicações cliente. Pode ser configurado para funcionar com seu sistema de autenticação e pode lidar com o controle de acesso usando regras de nível de campo com variáveis dinâmicas para seu sistema de autenticação.

Você também pode dar um _merge_ remotamente em esquemas GraphQL schemas e prover uma API GraphQL unificada.

![Arquitetura do Hasura GraphQL Engine](assets/hasura-arch.svg)

## Ferramental _Client-side_

Hasura funciona com qualquer cliente GraphQL. Recomendamos usar [o Apollo Client](https://github.com/apollographql/apollo-client). Veja [incrivel-graphql](https://github.com/chentsulin/awesome-graphql) para uma lista de clientes.

## Adicionando a lógica de negócio

O GraphQL Engine provê métodos amigáveis, escaláveis e performáticos para adicionar uma lógica de negócio customizada no seu _backend_:

### Esquemas remotos

Adiciona solucionadores customizados em um esquema remoto além do esquema GraphQL do Hasura, baseado em Postgres. 
Ideal para casos de uso como implementação e uma API de pagamentos ou pesquisar informações que não estão em sua base de dados - [leia mais](remote-schemas.md).

### Gatilhos _webhook_ em eventos do banco de dados

Adicionar lógica de negócio assíncrona, que é ativada baseado em eventos no banco de dados.
Ideal para notificações, _pipelines_ de dados do Postgres ou processamento assíncrono - [leia mais](event-triggers.md).

### Dados derivados ou transformações de dados

Transforme dados no Postgres ou rode a lógica de negócio para derivar outro conjunto de dados que pode ser buscado usando
o GraphQL Engine - [leia mais](https://docs.hasura.io/1.0/graphql/manual/queries/derived-data.html).

## Demonstrações

Confira todos nossas aplicações exemplo na
[comunidade/exemplos](community/examples) directory.

### Aplicações em tempo real

- Aplicação de chat de grupo feita em React, inclui um indicador de digitação, usuários online users e notificações de novas mensagens.
  - [Confira](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-chat)
  - [Lista de APIs](https://realtime-chat.demo.hasura.app/console)

- Aplicativo de localização em tempo real que mostra um veículo em movimento mudando suas coordenadas do GPS no mapa.
  - [Confira](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-location-tracking)
  - [Lista de APIs](https://realtime-location-tracking.demo.hasura.app/console)

- _Dashboard_ em tempo real para agregação de dados com alterações contínuas.
  - [Confira](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-poll)
  - [Lista de APIs](https://realtime-poll.demo.hasura.app/console)

### Vídeos

* [Adicionando GraphQL para uma instância do GitLab auto hospedada](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Aplicativo de Lista de Atividades com backend em Auth0 e GraphQL](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GraphQL no GitLab integrado com o serviço _GitLab auth_](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [_Dashboard_ para 10 milhões de corridas com geolocalização (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


## Suporte e Troubleshooting

As documentações e a comunidade vão te ajudar com a maioria dos problemas. 
Se tiver encontrado um _bug_ ou precisa entrar em contato conosco, pode utilizar os canais a seguir:

* Suporte e feedback: [Discord](https://discord.gg/vBPpJkS)
* Problemas & rastreamento de bugs: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Acompanhe os _updates_ do produto: [@HasuraHQ](https://twitter.com/hasurahq)
* Fale conosco em [website chat](https://hasura.io)

Estamos comprometidos ao fomento e à um ambiente aberto e acolhedor na comunidade. 
Por favor veja o [Código de Conduta](code-of-conduct.md).

Se quiser reportar um problema de segurança, por favor [leia isso](SECURITY.md).

## Contribuindo

Confira nosso [guia de contribuição](CONTRIBUTING.md) para maiores detalhes.

## Ativos de marketing

Os ativos de marketing de Hasura (logos, mascote Hasura, _powered by badges_ etc.) podem ser encontrados na pasta 
[assets/brand](assets/brand). Você é livre para utilizá-los em sua aplicação/website etc.
Adoraríamos que você adicionasse a insígnia "Powered by Hasura" em suas aplicações feitas com Hasura. ❤️

<div style="display: flex;">
  <img src="assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- Para backgrounds claros -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- Para backgrounds escuros -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## Licença

O núcleo GraphQL Engine está disponível sob a licença [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Todos os **outros conteúdos** (exceto aqueles nos diretórios [`server`](server), [`cli`](cli) e
[`console`](console)) estão disponíveis sob a licença [MIT License](LICENSE-community).
Isso inclui tudo nos diretórios [`docs`](docs) e [`community`](community).

## Traduções

Esse _readme_ está disponível em:

- [Japanese :jp:](translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [French :fr:](translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Portuguese :pt-br:]

Traduções para outros arquivos podem ser encontradas [aqui](translations).
