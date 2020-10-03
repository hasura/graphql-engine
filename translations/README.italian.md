# Hasura GraphQL Engine

[![Ultima release](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![Docs](https://img.shields.io/badge/docs-v1.x-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine è un server GraphQL fulmineo che ti fornisce **API GraphQL istantanee e realtime utilizzando Postgres**, inoltre integra [**webhook trigger**](../event-triggers.md) sugli eventi del database e [**schema remoti**](../remote-schemas.md) per gestire la logica di business.

Hasura ti aiuta a scrivere applicazioni [GraphQL](https://hasura.io/graphql/) supportate da Postgres o a migrare incrementalmente da applicazioni esistenti a GraphQL utilizzando Postgres.

Per saperne di più visita [hasura.io](https://hasura.io) e la [documentazione](https://hasura.io/docs/).

------------------

![Hasura GraphQL Engine Demo](../assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](../assets/demo.gif)

-------------------

## Caratteristiche

* **Scrivi query avanzate**: Filtraggio integrato, paginazione, ricerca di pattern, inserimento di massa, aggiornamento e cancellazione delle mutazioni
* **Realtime**: Converti qualsiasi query GraphQL in una query live utilizzando i subscription 
* **Unisci più remote schema**: Accedi agli schema GraphQL personalizzati per la logica di business attraverso un unico endpoint GraphQL Engine. [Ulteriori informazioni](../remote-schemas.md)
* **Trigger webhook o funzioni serverless**: Sugli eventi Postgres di inserimento/aggiornamento/cancellazione. [Ulteriori informazioni](../event-triggers.md).
* **Lavora live su database esistenti**: Collega GraphQL Engine ad un database Postgres esistente per ottenere istantaneamente un'API GraphQL.
* **Controllo dettagliato degli accessi**: Controllo dinamico degli accessi integrabile al tuo sistema di autenticazione (ad es.: auth0, firebase-auth)
* **Alte prestazioni & low-footprint**: Immagine Docker da ~15MB; ~50MB RAM @ 1000 richieste/s; sfrutta CPU multi-core
* **Interfaccia di amministrazione & Migrazioni**: Interfaccia di amministrazione & migrazioni degli schema ispirati a Rails
* **Postgres** ❤️: Supporto dei tipi di Postgres (PostGIS/geo-location, ecc.), trasformazione delle viste in *grafi*, attivazione di funzioni o procedure memorizzate tramite le mutazioni

Per saperne di più visita [hasura.io](https://hasura.io) e la [documentazione](https://hasura.io/docs/).

## Indice
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Indice**

- [Introduzione rapida:](#quickstart)
    - [Deploy su Hasura Cloud in un click](#one-click-deployment-on-hasura-cloud)
    - [Altre opzioni per deploy in un click](#other-one-click-deployment-options)
    - [Altri metodi di deploy](#other-deployment-methods)
- [Architettura](#architecture)
- [Strumenti lato client](#client-side-tooling)
- [Implementa la logica di business](#add-business-logic)
    - [Schema remoti](#remote-schemas)
    - [Trigger webhook sugli eventi del database](#trigger-webhooks-on-database-events)
- [Demo](#demos)
    - [Applicazioni realtime](#realtime-applications)
    - [Video](#videos)
- [Supporto & risoluzione dei problemi](#support--troubleshooting)
- [Contribuire](#contributing)
- [Asset del brand](#brand-assets)
- [Licenza](#license)
- [Traduzioni](#translations)

<!-- markdown-toc end -->

## Introduzione rapida:

### Deploy su Hasura Cloud in un click

Il modo più facile e veloce per provare Hasura è attraverso [Hasura Cloud](https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html).

1. Clicca sul seguente bottone per lanciare il deploy dell'engine GraphQL su Hasura Cloud incluso l'add-on Postgres o utilizzando un database Postgres esistente:

    [![Deploy to Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

2. Apri la console Hasura

   Clicca sul bottone "Launch console" per aprire la console Hasura.

3. Scrivi la tua prima query GraphQL

   Crea una tabella e lancia subito la tua prima query. Segui questa [semplice guida](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html).


### Altre opzioni per deploy in un click

Puoi trovare di seguito altre guide per lanciare il deploy di Hasura con un click:

| **Infra provider** | **Link rapido** | **Informazioni aggiuntive** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/render-one-click.html) |
________

### Altri metodi di deploy

Per deploy basati su Docker e opzioni di configurazione avanzate, leggi le [guide
al deploy](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) o
i [manifesti di installazione](../install-manifests).

``
## Architettura

Il GraphQL Engine di Hasura è il front-end di un'istanza del database Postgres e può accettare richieste di GraphQL dalle applicazioni dei tuoi client. Può essere configurato per utilizzare un sistema di autenticazione esistente e può gestire il controllo degli accessi dal tuo sistema di autenticazione usando regole a livello di campo con variabili dinamiche.

È anche possibile unire degli schema remoti di GraphQL e fornire un'API GraphQL unificata.

![Hasura GraphQL Engine architecture](../assets/hasura-arch.svg)

## Strumenti lato client

Hasura funziona con qualsiasi client GraphQL. Si consiglia di utilizzare [Apollo Client](https://github.com/apollographql/apollo-client). Vedi [awesome-graphql](https://github.com/chentsulin/awesome-graphql) per una lista di client.

## Implementa la logica di business

GraphQL Engine fornisce metodi semplici, scalabili e performanti per aggiungere una logica di business personalizzata al vostro backend:

### Schema remoti

Oltre agli schema GraphQL di Hasura basati su Postgres, è possibile aggiungere resolver personalizzati per gli schema remoti. Questi sono ideali per casi d'uso come l'implementazione di un API di pagamento o per interrogare dati che non si trovano nel tuo database - [leggi tutto](../remote-schemas.md)

### Trigger webhook sugli eventi del database

È possibile aggiungere logiche di business asincrone che vengono attivate in reazione agli eventi del database.
Queste sono ideali per notifiche, pipelines dati da Postgres o elaborazioni asincrone - [leggi tutto](../event-triggers.md).

### Dati derivati o trasformazioni sui dati

Trasforma i dati in Postgres o esegui la logica di business sui di essi per ricavare un altro dataset che può essere interrogato utilizzando GraphQL Engine - [leggi tutto](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## Demo

Dai un'occhiata alle applicazioni d'esempio nella cartella [community/sample-apps](../community/sample-apps).

### Applicazioni realtime

- Chat di gruppo scritta in React; implementa notifiche per i nuovi messaggi, l'indicatore di 
  digitazione e la visualizzazione degli utenti online.
  - [Provala ora](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-chat)
  - [Browse APIs](https://realtime-chat.demo.hasura.app/console)

- Applicazione di localizzazione in tempo reale che traccia e visualizza le coordinate di un veicolo su una mappa.
  - [Provala ora](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-location-tracking)
  - [Esplora le API](https://realtime-location-tracking.demo.hasura.app/console)

- Dashboard realtime per l'aggregazione di dati in continuo cambiamento.
  - [Provala ora](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-poll)
  - [Esplora le API](https://realtime-poll.demo.hasura.app/console)

### Video

* [Aggiungere GraphQL ad un istanza self-hosted di GitLab](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 minuti*)
* [Applicazione Todo con Auth0 e backend GraphQL](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 minuti*)
* [GraphQL su GitLab con autenticazione GitLab integrata](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 minuti*)
* [Dashboard per 10 milioni di veicoli con geo-coordinate (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 minuti*)


## Support & Troubleshooting

La documentazione e la comunità ti aiuteranno a risolvere la maggior parte dei problemi. Se hai incontrato un bug o desieri contattarci, puoi trovarci utilizzando uno dei seguenti canali:

* Supporto & feedback: [Discord](https://discord.gg/hasura)
* Problemi & segnalazioni di bug: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Segui gli aggiornamenti del prodotto: [@HasuraHQ](https://twitter.com/hasurahq)
* Scrivici nella [chat del nostro sito](https://hasura.io)

Ci adoperiamo per promuovere un ambiente aperto e accogliente nella comunità. Si prega di consultare il [Codice di condotta](../code-of-conduct.md).

Se si desidera segnalare un problema di sicurezza, si prega di [leggere questo](../SECURITY.md).

## Contribuire

Consulta la nostra [guida per contribuire](../CONTRIBUTING.md) per maggiori dettagli.

## Asset del brand

Asset del brand Hasura (loghi, a mascot di Hasura, powered by badges ecc.) possono
essere trovati nella cartella [../assets/brand](assets/brand) folder. 
Gli asset possono essere inseriti liberamente in applicazioni, siti web, ecc. 

Saremmo entusiasti se aggiungessi il badge "Powered by Hasura".
alle tue applicazioni realizzate con Hasura. ❤️

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

## Licenza

Il GraphQL Engine è disponibile sotto la [Licenza Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Tutti gli **altri contenuti** (ad eccezione di quelli presenti nelle cartelle 
[`server`](../server), [`cli`](../cli) e [`console`](../console)) sono disponibili sotto la 
[Licenza MIT](../LICENSE-community).
Questo include tutti i file nelle cartelle [`docs`](../docs) e [`community`](../community).

## Traduzioni

This readme is available in the following translations:

- [Inglese :gb:](https://github.com/hasura/graphql-engine/blob/stable/README.md)
- [Giapponese :jp:](README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [Francese :fr:](README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Bosniaco :bosnia_herzegovina:](README.bosnian.md) (:pray: [@hajro92](https://github.com/hajro92))
- [Russo :ru:](README.russian.md) (:pray: [@highflyer910](https://github.com/highflyer910))
- [Greco 🇬🇷](README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))
- [Spagnolo 🇲🇽](README.mx_spanish.md)(:pray: [@ferdox2](https://github.com/ferdox2))
- [Indonesiano :indonesia:](README.indonesian.md) (:pray: [@anwari666](https://github.com/anwari666))
- [Portoghese    :brazil:](README.portuguese_br.md) (:pray: [@rubensmp](https://github.com/rubensmp))
- [Tedesco 🇩🇪](README.german.md) (:pray: [@FynnGrandke](https://github.com/FynnGrandke))
- [Cinese :cn:](README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))
- [Turco :tr:](README.turkish.md) (:pray: [@berat](https://github.com/berat))
- [Coreano :kr:](README.korean.md) (:pray: [@라스크](https://github.com/laskdjlaskdj12))

Translations for other files can be found [here](translations).
