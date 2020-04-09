# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)

<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine é um servidor GraphQL extremamente rápido que fornece **instantaneamente, e em tempo real APIs GraphQL no Postgres**, com [**webhook triggers**](/event-triggers.md) em eventos de banco de dados, e esquemas remotos para lógica de negócios.

Hasura ajuda a criar GraphQL apps suportados pelo Postgres ou migrar gradualmente para GraphQL apps existentes usando Postgres.

Leia mais em [hasura.io](https://hasura.io) e na [documentação](https://hasura.io/docs).

---

![Hasura GraphQL Engine Demo](../assets/demo.gif)

---

![Hasura GraphQL Engine Realtime Demo](../assets/realtime.gif)

---

## Funcionalidades

- **Faça consultas poderosas**: Filtragem embutida, paginação, pesquisa de padrões, inserção em massa, atualização, exclusão de mutações.
- **Em tempo real**: Converta qualquer consulta GraphQL em uma consulta ativa usando subscriptions.
- **Mesclar esquemas remotos**: Acesse esquemas GraphQL personalizados para lógica de negócios por meio de um único GraphQL Engine endpoint. [Leia mais](/remote-schemas.md).
- **Acionar webhooks ou funções sem servidor**: No Postgres insira/atualize/exclua eventos ([leia mais](/event-triggers.md))
- **Funciona com bando de dados existentes**: Aponte para um banco de dados Postgres existente para obter uma GraphQL API pronto para uso.
- **Controle de acesso refinado**: Controle de acesso dinâmico que se integra ao seu sistema de autenticação(ex: auth0, firebase-auth)
- **Alta performance & low-footprint**: ~15MB docker image; ~50MB RAM @ 1000 req/s; multicore aware
- **Interface de Admin & Migração**: Interface de Admin & esquemas de migração inspirada em Rails
- **Postgres** ❤️: Suporta Postgres types(PostGIS/geo-location, etc.), transforma visualizações em _graphs_, aciona funções ou procedimentos armazenados com mutações

Leia mais em [hasura.io](https://hasura.io) e na [documentação](https://hasura.io/docs).

## Índice

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Índice**

- [Começo rápido](#começo-rápido)
  - [Implantação no Heroku em um click](#implantação-no-heroku-em-um-click)
  - [Outros métodos de implantação](#outros-métodos-de-implantação)
- [Arquitetura](#arquitetura)
- [Ferramentas do client](#ferramentas-do-client)
- [Adicionar lógica de negócios](#adicionar-lógica-de-negócios)
  - [Esquemas remotos](#esquemas-remotos)
  - [Acionar webhooks em eventos de banco de dados](#acionar-webhooks-em-eventos-de-banco-de-dados)
  - [Dados derivados ou transformações de dados](#dados-derivados-ou-transformações-de-dados)
- [Demos](#demos)
  - [Aplicações em tempo real](#aplicações-em-tempo-real)
  - [Vídeos](#vídeos)
- [Suporte e Solução de problemas](#suporte-e-solução-de-problemas)
- [Contribuindo](#contribuindo)
- [Recursos da marca](#recursos-da-marca)
- [Licença](#licença)

<!-- markdown-toc end -->

## Começo rápido:

### Implantação no Heroku em um click

O modo mais fácil de experimentar Hasura é via o Heroku.

1. Clique no botão abaixo para implantar GraphQL Engine no Heroku com o complemento gratuito do Postgres:

   [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Abra o console do Hasura

   Visite `https://<app-name>.herokuapp.com` (_substitua \<app-name\> com o nome do seu app_) para abrir o console do admin.

3. Faça sua primeira consulta GraphQL

   Crie uma tabela e rode instantaneamente sua primeira consulta. Siga esse [simples guia](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Outros métodos de implantação

Confira as instruções para as seguintes opções de implantação com um clique:

| **Fornecedor de infraestrutura** |                                                                                                                       **Link com um clique**                                                                                                                       |                                                             **Informação adicional**                                                              |
| :------------------------------: | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: | :-----------------------------------------------------------------------------------------------------------------------------------------------: |
|           DigitalOcean           |                  [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme)                  | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
|              Azure               | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) |                    [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html)                    |

### Outros métodos de implantação

Para implantação baseada no Docker e opções de configuração avançadas, veja o [guias de implantação](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) ou o
[manifestos de instalação](/install-manifests).

## Arquitetura

O Hasura GraphQL Engine possui uma instância de banco de dados Postgres e pode aceitar requisições GraphQL de seus aplicativos clientes. Ele pode ser configurado para funcionar com seu sistema de autenticação existente e pode manipular o controle de acesso usando regras em nível de campo com variáveis dinâmicas do seu sistema de autenticação.

Você também pode mesclar esquemas remotos do GraphQL e fornecer uma API GraphQL unificada.

![Hasura GraphQL Engine architecture](/assets/hasura-arch.svg)

## Ferramentas do client

Hasura trabalha com qualquer client GraphQL. Recomendamos o uso do [Apollo Client](https://github.com/apollographql/apollo-client). Veja [awesome-graphql](https://github.com/chentsulin/awesome-graphql) para uma lista de clients.

## Adicionar lógica de negócios

O GraphQL Engine fornece métodos easy-to-reason, escaláveis e de alto desempenho para adicionar lógica comercial personalizada ao seu back-end:

### Esquemas remotos

Adicione resolvers personalizados em um esquema remoto, além do esquema GraphQL baseado em Postgres do Hasura. Ideal para casos de uso, como implementar uma API de pagamento ou consultar dados que não estão no seu banco de dados - [leia mais](/remote-schemas.md).

### Acionar webhooks em eventos de banco de dados

Adicione lógica de negócios assíncrona que é acionada com base nos eventos do banco de dados.Ideal para notificações, pipelines de dados do Postgres ou processamento assíncrono - [leia mais](/event-triggers.md).

### Dados derivados ou transformações de dados

Transforme dados no Postgres ou execute a lógica de negócios para obter outro conjunto de dados que possa ser consultado usando o GraphQL Engine - [leia mais](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## Demos

Confira todos os aplicativos de exemplo no diretório
[community/examples](../community/sample-apps).

### Aplicações em tempo real

- O aplicativo de bate-papo em grupo criado com o React inclui um indicador de digitação, usuários online e notificações de novas mensagens.

  - [Experimente](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-chat)
  - [Browse APIs](https://realtime-chat.demo.hasura.app/console)

- Aplicativo de rastreamento de localização ao vivo que mostra um veículo em movimento alterando as coordenadas atuais do GPS em movimento no mapa.

  - [Experimente](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-location-tracking)
  - [Browse APIs](https://realtime-location-tracking.demo.hasura.app/console)

- Um painel em tempo real para agregações de dados em constante mudança de dados.
  - [Experimente](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-poll)
  - [Browse APIs](https://realtime-poll.demo.hasura.app/console)

### Vídeos

- [Adicionar o GraphQL a uma instância auto-hospedada do GitLab](https://www.youtube.com/watch?v=a2AhxKqd82Q) (_3:44 mins_)
- [Todo app com Auth0 e GraphQL backend](https://www.youtube.com/watch?v=15ITBYnccgc) (_4:00 mins_)
- [GraphQL no GitLab integrado ao GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (_4:05 mins_)
- [Painel para 10 milhões de viagens com geolocalização (PostGRES, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (_3:06 mins_)

## Suporte e Solução de problemas

A documentação e a comunidade ajudarão você a solucionar a maioria dos problemas. Se você encontrou um bug ou precisa entrar em contato conosco, entre em contato usando um dos seguintes canais:

- Support & feedback: [Discord](https://discord.gg/vBPpJkS)
- Issue & bug tracking: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
- Siga a atualizações do produto: [@HasuraHQ](https://twitter.com/hasurahq)
- Fale conosco em nosso [chat](https://hasura.io)

Estamos comprometidos em promover um ambiente aberto e acolhedor na comunidade. Por favor, consulte o [Código de Conduta](/code-of-conduct.md).

Se você deseja relatar um problema de segurança, por favor [leia isto](/SECURITY.md).

## Contribuindo

Confira nosso [guia de contribuição](/CONTRIBUTING.md) para mais detalhes.

## Recursos da marca

Os ativos da marca Hasura (logotipos, o mascote Hasura, powered by badges etc.) podem ser encontrados na pasta [assets/brand](../assets/brand). Sinta-se à vontade para usá-los em seu aplicativo / site, etc. Ficaríamos felizes se você adicionar o "Powered by Hasura" badge para seus aplicativos criados usando o Hasura. ❤️

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

## Licença

O core GraphQL Engine está disponível sob o [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Todos **os outros conteúdos** (exceto aqueles em [`server`](../server), [`cli`](../cli) e
[`console`](../console) diretórios) estão disponíveis sob o [MIT License](/LICENSE-community).
Isso inclui tudo nos diretórios [`docs`](../docs) e [`community`](../community).
