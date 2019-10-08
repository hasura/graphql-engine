# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)

<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine é um servidor GraphQL extremamente rápido que lhe permite criar **instantâneamente, desde APIs GraphQL realtime com Postgres**, até [**gatilhos webhook**](event-triggers.md) em eventos de banco de dados, e [**esquemas remotos**](remote-schemas.md) para lógicas de negócio.

Hashura te ajuda a criar apps com GraphQL e Postgres ou incrementalmente mudar uma aplicação existente que use Postgres.

Leia mais em [hasura.io](https://hasura.io) e as [docs](https://docs.hasura.io).

---

![Hasura GraphQL Engine Demo](../assets/demo.gif)

---

![Hasura GraphQL Engine Realtime Demo](../assets/realtime.gif)

---

## Features

- **Faça queries poderosas**: Filtros embutidos, paginação, pesquisa por patterns, bulk insert, update, delete mutations
- **Tempo real**: Converta qualquer query Graphql para uma query em tempo real usando subscriptions
- **Combine esquemas remotos**: Acesse esquemas GraphQL específicos para a sua lógica de negócio por um único endpoint do GraphQL Engine [**Leia mais**](remote-schemas.md).
- **Ative webhooks e funções serverless**: No Postgres eventos de insert/update/delete ([leia mais](event-triggers.md))
- **Funciona com bancos de dados em produção**: Aponte o GraphQL Engine para uma instância Postgres existente e obtenha instantâneamente uma API GraphQL pronta para uso.
- **Controle de acesso classe A**: Controle de cesso dinâmico que se integra com seu sistema de autenticação (ex: auth0, firebase-auth)
- **Alta performance e baixo impacto**: Imagem docker com ~15MB; ~50MB RAM @ 1000 req/s; compatível com multi-core
- **Painel de Admin e Migrations**: Painel de Admin e esquema de migrations inspirado no Rails
- **Postgres** ❤️: Suporta os tipos do Postgress (PostGIS/geo-location, etc.), transforma views em _graphs_, gatilhos e funções em mutations

Leia mais em [hasura.io](https://hasura.io) e nas [docs](https://docs.hasura.io).

## Índice

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Inicio rápido:](#inicio-r%c3%a1pido)
  - [Deploy com um click na Heroku](#deploy-com-um-click-na-heroku)
  - [Outras opções de deploy com um clique](#outras-op%c3%a7%c3%b5es-de-deploy-com-um-clique)
  - [Outros métodos de deploy](#outros-m%c3%a9todos-de-deploy)
- [Arquitetura](#arquitetura)
- [Ferramentas para o client-side](#ferramentas-para-o-client-side)
- [E lógica de negócio](#e-l%c3%b3gica-de-neg%c3%b3cio)
  - [Esquemas remotos](#esquemas-remotos)
  - [Acione webhooks em eventos do banco de dados](#acione-webhooks-em-eventos-do-banco-de-dados)
  - [Dado derivado ou transformação do dado](#dado-derivado-ou-transforma%c3%a7%c3%a3o-do-dado)
- [Demos](#demos)
  - [Realtime applications](#realtime-applications)
  - [Videos](#videos)
- [Suporte e resolução de problemas](#suporte-e-resolu%c3%a7%c3%a3o-de-problemas)
- [Contribuindo](#contribuindo)
- [Recursos da marca](#recursos-da-marca)
- [Licenças](#licen%c3%a7as)
- [Traduções](#tradu%c3%a7%c3%b5es)

<!-- markdown-toc end -->

## Inicio rápido:

### Deploy com um click na Heroku

A maneira mais rápida de usar o Hasura é pela Heroku:

1. Click no botão abaixo para fazer deploy do GraphQL Engine no Heroku com o add-on grátis do Postgress:
2. [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

3. Abra o console do Hasura

   Visite `https://<app-name>.herokuapp.com` (_troque \<app-name\> com o nome do seu app_) para abrir o console de administror.

4. Faça sua primeira query GraphQL

   Crie uma tabela e rode sua primeira query instantâneamente. Siga esse [guia simples](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Outras opções de deploy com um clique

Confira as instruções para as seguintes opções de deploy com clique:

| **Provedor** |                                                                                                                         **One-click link**                                                                                                                         |                                                            **Informações adicionais**                                                             |
| :----------: | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: | :-----------------------------------------------------------------------------------------------------------------------------------------------: |
| DigitalOcean |                  [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme)                  | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
|    Azure     | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) |                    [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html)                    |

### Outros métodos de deploy

Para aplicações baseadas no docker e configurações avançadas, veja [guias de deploy](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html) ou
[manifestos de instalação](install-manifests).

## Arquitetura

O Hasura GraphQL Engine expõe uma instância Postgress e pode aceitar requests GraphQL dos seus apps clientes. Ele pode ser configurado para funcionar com seu sistema de autenticação existente, além de poder gerenciar o controle de acesso usando regras variáveis dinâmicas do seu sistema de autenticação.

Vocês também pode combinar esquemas remotos do GraphQL e prover uma API unificada.

![Arquitetura do Hasura GraphQL Engine](../assets/hasura-arch.svg)

## Ferramentas para o client-side

O Hasura funciona com qualquer cliente GraphQL. Nós recomendamos o uso do [Apollo Client](https://github.com/apollographql/apollo-client). Veja [awesome-graphql](https://github.com/chentsulin/awesome-graphql) para uma lista de clientes.

## E lógica de negócio

O GraphQL Engine provê métodos fáceis de esquematizar, escaláveis e performáticos para a adição de lógicas de negócio customizadas ao seu backend.

### Esquemas remotos

Adicione resolvedores customizados em um esquema remoto em adição ao esquema GraphQL da Hasure baseado em Postgres. Ideal para casos como a implementação de uma API de pagamentos, ou busca de dados que não está no seu banco de dados - [leia mais](remote-schemas.md).

### Acione webhooks em eventos do banco de dados

Adicione lógicas de negócio que são ativadas baseadas em eventos no banco de dados.
Ideal para notificações, pipelines de dados do Postgres ou processamento
assincrono - [leia mais](event-triggers.md).

### Dado derivado ou transformação do dado

Transforme dados no Postgres ou rode lógicas de negócio sobre eles para criar um novo dataset que pode ser buscado usando o GraphQL Engine - [leia mais](https://docs.hasura.io/1.0/graphql/manual/queries/derived-data.html).

## Demos

Confira todas as aplicações de exemplo no diretório
[community/examples](community/examples).

### Realtime applications

- Aplicação de Chat em grupo com React, incluindo indicação de digitação, usuários online e
- notificação de novas mensagens.

  - [Testar agora](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-chat)
  - [Ver APIs](https://realtime-chat.demo.hasura.app/console)

- App de localização em tempo real que mostra um carro em movimento alterando
- as coordenadas de GPS enquanto se move em um mapa

  - [Testar agora](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-location-tracking)
  - [Ver APIs](https://realtime-location-tracking.demo.hasura.app/console)

- Uma dashboard em tempo real para agregação de dados em uma base em continua mudança.
  - [Testar agora](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-poll)
  - [Ver APIs](https://realtime-poll.demo.hasura.app/console)

### Videos

- [Add GraphQL to a self-hosted GitLab instance](https://www.youtube.com/watch?v=a2AhxKqd82Q) (_3:44 mins_)
- [Todo app with Auth0 and GraphQL backend](https://www.youtube.com/watch?v=15ITBYnccgc) (_4:00 mins_)
- [GraphQL on GitLab integrated with GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (_4:05 mins_)
- [Dashboard for 10million rides with geo-location (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (_3:06 mins_)

## Suporte e resolução de problemas

A documentação e comunidade irão ajudá-lo a solucionar a maioria dos problemas. Se você encontrou um bug ou necessita de entrar em contato conosco, você pode nos contatar usando um dos seguintes canais:

- Suporte e feedback: [Discord](https://discord.gg/vBPpJkS)
- Issues e bug tracking: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
- Siga as atualizações do produto: [@HasuraHQ](https://twitter.com/hasurahq)
- Fale conosco em nosso [chat](https://hasura.io)

Nós estamos empenhados em fomentar um ambiente aberto e acolhedor na comunidade. Por favor, veja o [Código de conduta](code-of-conduct.md).

Se você deseja reportar um problema de segurança, por favor [leia isso](SECURITY.md).

## Contribuindo

Confira o [guia de contribuição](CONTRIBUTING.md) para mais.

## Recursos da marca

Os recursos da marca Hasura (logos, o mascote da Hasura, badges de 'powered by' etc.) podem
ser encontrados na pasta [assets/brand](../assets/brand). Sinta-se livre para usá-los na sua
aplicação/website etc. Nós ficaríamos contentes se você adicionasse o badge "Powered by Hasura"
na sua aplicação feita usando Hasusa. ❤️

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- For light backgrounds -->
<a href="https://hasura.io">
  <img
    width="150px"
    src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg"
  />
</a>

<!-- For dark backgrounds -->
<a href="https://hasura.io">
  <img
    width="150px"
    src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg"
  />
</a>
```

## Licenças

O núcleo do GraphQL Engine está disponível sob a [Licença Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Todos os **outros conteúdos** (exceto aqueles nos diretórios [`server`](server), [`cli`](cli) e
[`console`](console)) estão disponíveis sob a [Licença MIT](LICENSE-community).
Isso inclui tudo nos diretórios [`docs`](docs) e [`community`](community).

## Traduções

Esse readme está disponível nas seguintes linguagens:

- [English :uk:](.../README.md)
- [Japanese :jp:](translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [French :fr:](translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))

As traduções para outros arquivos podem ser encontradas [aqui](./).
