# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine é um servidor extremamente rápido que oferece a você criar **instantâneamente, em tempo real, APIs GraphQL baseadas no Postgres**, com [**webhook triggers**](../event-triggers.md) em eventos do banco de dados, e [**remote schemas**](../remote-schemas.md) para a regra de negócio.

Hasura te ajuda a contruir aplicativos GraphQL suportados pelo Postgres ou migrar uma aplicação já existente, que utiliza Postgres, para GraphQL.

Leia mais em [hasura.io](https://hasura.io) e na [documentação](https://docs.hasura.io).

------------------

![Hasura GraphQL Engine Demo](../assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](../assets/realtime.gif)

-------------------

## Funcionalidades

* **Realize queries avançadas**: filtragem, paginação, busca de padrões, inserção em massa, atualizações, exclusões
* **Em Tempo-Real**: Converta qualquer consulta do GraphQL em uma consulta em tempo real usando as assinaturas.
* **Faça o Merge de Schemas remotos**: Acesse seus próprios esquemas GraphQL para sua lógica de negócios através de um único ponto de acesso GraphQL. [**Leia Mais**](../remote-schemas.md).
* **Disparar webhooks ou funções sem servidor**: Em resposta aos eventos do Postgres, insira / atualize / exclua ([leia mais](../event-triggers.md))
* **Funciona com banco de dados já existentes**: direcione o GraphQL Engine para um banco de dados Postgres existente para obter instantaneamente uma API GraphQL pronta para uso.
* **Controle de acesso detalhado**: Controle de acesso dinâmico que integra com o seu sistema de autorização (ex: auth0, firebase-auth)
* **Alto desempenho e baixo impacto**: ~15MB de imagem do Docker; ~50MB RAM @ 1000 req/s; levando em conta o multi-core
* **Interface de administração e migração**: Interface de administração e migração de schemas inspirados no Rails
* **Postgres** ❤️: suporta tipos do Postgres (PostGIS / localização geográfica, etc.), transforma visualizações em gráficos, aciona procedimentos ou funções armazenados através de mutações

Leia mais em [hasura.io](https://hasura.io) e a [documentação](https://docs.hasura.io).

## Conteúdo
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Conteúdo**

- [Primeiros passos:](#primeiros-passos)
    - [Deploy no Heroku em um único clique](#deploy-no-heroku-em-um-único-clique)
    - [Outros métodos de deploy](#outras-opções-de-deploy-em-um-único-clique)
- [Arquitetura](#arquitetura)
- [Ferramentas de Client](#ferramentas-de-client)
- [Adicione a regra de negócio](#adicione-a-regra-de-negócio)
    - [Schemas remotos](#schemas-remotos)
    - [Acionar webhooks em eventos do banco de dados](#acionar-webhooks-em-eventos-do-banco-de-dados)
- [Demos](#demos)
    - [Aplicações em tempo real](#aplicações-em-tempo-real)
    - [Videos](#videos)
- [Suporte e solução de problemas](#suporte-e-solução-de-problemas)
- [Contribuindo](#contribuindo)
- [Elementos da marca](#elementos-da-marca)
- [Licença](#licença)

<!-- markdown-toc end -->

## Primeiros passos:

### Deploy no Heroku em um único clique

A maneira mais rápida de experimentar o Hasura é via Heroku.

1. Clique no botão abaixo para implantar o GraphQL Engine no Heroku com o add-on gratuito do Postgres:

    [![Deploy no Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Abra o Hasura console

   Visite `https://<app-name>.herokuapp.com` (*substitua \<app-name\> pelo nome do seu aplicativo*) para abrir o console de administrador.

3. Faça sua primeira consulta em GraphQL

   Crie sua tabela e logo em seguida executa sua primeira consulta. Acompanhe este [guia simples](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Outras opções de deploy em um único clique

Check out the instructions for the following one-click deployment options:

| **Provedor da Nuvem** | **Link único-clique** | **Informação Adicional** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](https://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |

### Outros métodos de deploy

Para métodos de implantação baseados no Docker e opções avançadas de configuração, consulte [guias de deploy](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html) ou
[manifestos de instalação](../install-manifests).

## Arquitetura

O Hasura GraphQL Engine está localizado na frente de um banco de dados Postgres e pode aceitar consultas GraphQL de seus aplicativos clientes. Ele pode ser configurado para funcionar com seu sistema de autenticação existente e pode lidar com o controle de acesso em nível de campo por meio de regras, com variáveis dinâmicas provenientes do sistema de autenticação.

Você também pode mergear esquemas remotos do GraphQL e fornecer uma API GraphQL unificada.

![Hasura GraphQL Engine architecture](../assets/hasura-arch.svg)

## Ferramentas de Client

Hasura funciona com qualquer GraphQL client. Nós recomendamos o uso do [Apollo Client](https://github.com/apollographql/apollo-client). Consulte [awesome-graphql](https://github.com/chentsulin/awesome-graphql) para uma lista de clients disponíveis.

## Adicione a regra de negócio

O GraphQL Engine fornece várias maneiras claras, escaláveis e poderosas de adicionar sua própria regra de negócios ao seu back-end:

### Schemas remotos

Adicione resolvers personalizados em um esquema remoto, além do esquema GraphQL baseado em Postgres do Hasura. Ideal para casos de uso, como por exemplo implementar uma API de pagamento ou consultar dados que não estão no seu banco de dados - [leia mais](../remote-schemas.md).

### Acionar webhooks em eventos do banco de dados

Adicione lógica de negócios assíncrona acionada por eventos do banco de dados. Ideal para notificações, pipelines de dados Postgres ou processamento assíncrono - [leia mais](../event-triggers.md).

### Dados derivados ou transformações de dados

Transforme os dados em Postgres ou execute a regra de negócios para obter outro conjunto de dados que possa ser consultado usando o GraphQL Engine- [leia mais](https://docs.hasura.io/1.0/graphql/manual/queries/derived-data.html).

## Demos

Confira todos os exemplos da aplicação na pasta
[community/examples](../community/sample-apps).

### Aplicações em tempo real

- Um aplicativo de mensagens em grupo desenvolvido com o React, incluindo um indicador de digitação, usuários conectados e notificações de novas mensagens.
  - [Experimente](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-chat)
  - [Explorar APIs](https://realtime-chat.demo.hasura.app/console)

- Aplicativo de localização em tempo real mostrando um veículo cujas coordenadas GPS estão se movendo em um mapa.
  - [Experimente](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-location-tracking)
  - [Explorar APIs](https://realtime-location-tracking.demo.hasura.app/console)

- Um dashboard em tempo real de dados em constante mudança.
  - [Experimente](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-poll)
  - [Explorar APIs](https://realtime-poll.demo.hasura.app/console)

### Videos

* [Adicione o GraphQL a uma instância auto-hospedada do GitLab](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Aplicativo de tarefas utilizando Auth0 e GraphQL backend](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [API GraphQL no GitLab integrada à autenticação GitLab](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [Dashboard de 10 milhões de trajetos utilizando geo-localização (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


## Suporte e solução de problemas

A documentação e a comunidade ajudarão você a resolver a maioria dos problemas. Se você encontrou um bug ou precisa entrar em contato, pode entrar em contato através dos seguintes canais:

* Suporte e feedback: [Discord](https://discord.gg/vBPpJkS)
* Problemas e bugs: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Confira atualizações: [@HasuraHQ](https://twitter.com/hasurahq)
* Fale conosco pelo [website chat](https://hasura.io)

Estamos envolvidos no desenvolvimento de um ambiente aberto e acolhedor na comunidade. Por favor, consulte o [Código de Conduta](../code-of-conduct.md).

Se você quer reportar um problema de segurança, por favor [leia aqui](../SECURITY.md).

## Contribuindo
Confira nosso [guia de contribuição](../CONTRIBUTING.md) para mais detalhes.

## Elementos da marca

Os elementos da marca Hasura (logotipos, o mascote Hasura, emblemas "feito por" etc.) podem ser encontrados no diretório [assets/brand](../assets/brand). Sinta-se à vontade para usá-los em seu aplicativo / site, etc. Ficaríamos felizes se você adicionar o selo "Powered by Hasura" ao seu aplicativo desenvolvido com o Hasura. ❤️

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

## Licença

O core do GraphQL Engine está disponível sob a [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Todos os **outros conteúdos** (exceto os diretórios [`server`](../server), [`cli`](../cli) e
[`console`](../console)) estão disponíveis sob licença [MIT License](../LICENSE-community). Isso inclui todo o conteúdo dos [`docs`](../docs) e [`community`](../community).
