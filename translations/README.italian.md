# Hasura GraphQL Engine

[![Latest release](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![Docs](https://img.shields.io/badge/docs-v2.x-brightgreen.svg?style=flat)](https://hasura.io/docs)

<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://hasura.io/newsletter/"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura è un prodotto open source che accelera lo sviluppo API di 10 volte fornendoti all'istante API [GraphQL](https://hasura.io/graphql/) o REST con autorizzazione integrata.

Leggi di più su [hasura.io](https://hasura.io) e nei [docs](https://hasura.io/docs/).

------------------

![Hasura GraphQL Engine Demo](../assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](../assets/realtime.gif)

-------------------

## Features

* **Crea queries potenti**: Filtri, paginazione, ricerca di pattern, e mutazioni di inserimento, aggiornamento ed eliminazione, anche in bulk
* **Lavora con live database esistenti**: Punta a un database esistente per ottenere istantaneamente una GraphQL API pronta all'uso
* **In tempo reale**: Converti qualsiasi query GraphQL in una query live usando le sottoscrizioni
* **Merge di schemi remoti**: Accedi a schemi GraphQL personalizzati per la business logic tramite un singolo endpoint GraphQL Engine. [**Leggi di più**](remote-schemas.md).
* **Estendibile con le Azioni**: Scrivi API REST per estendere lo schema di Hasura con una business logic personalizzata.
* **Attiva webhook o funzioni serverless**: Eventi di inserimento/aggiornamento/eliminazione su Postgres ([leggi di più](event-triggers.md))
* **Trigger programmati**: Esegui la business logic personalizzata in momenti specifici utilizzando una configurazione cron o un singolo evento.
* **Controllo degli accessi preciso**: Controllo accessi dinamico che si integra con il tuo sistema di autenticazione (es: auth0, firebase-auth)
* **Admin UI & Migrazioni**: Interfaccia utente di amministrazione & migrazioni degli schemi ispirate a Rails.
* **Database Supportati**: Supporta PostgreSQL (e le sue varianti), MS SQL Server e Big Query. Supporto per altri [database](https://hasura.io/graphql/database/) in arrivo.

Leggi di più su [hasura.io](https://hasura.io) e nei [docs](https://hasura.io/docs/).

## Indice dei contenuti
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Sommario**

- [Guida introduttiva:](#quickstart)
    - [Deployment one-click su Hasura Cloud](#one-click-deployment-on-hasura-cloud)
    - [Altre opzioni di deployment one-click](#other-one-click-deployment-options)
    - [Altri metodi di deployment](#other-deployment-methods)
- [Architettura](#architecture)
- [Strumenti client-side](#client-side-tooling)
- [Aggiungere la business logic](#add-business-logic)
    - [Schemi remoti](#remote-schemas)
    - [Invoca webhook su eventi del database](#trigger-webhooks-on-database-events)
- [Demo](#demos)
    - [Applicazioni in tempo reale](#realtime-applications)
    - [Video](#videos)
- [Supporto e Risoluzione dei Problemi](#support--troubleshooting)
- [Resta aggiornato](#stay-up-to-date)
- [Contribuire](#contributing)
- [Asset del Marchio](#brand-assets)
- [Licenza](#license)
- [Traduzioni](#translations)
 
<!-- markdown-toc end -->

## Guida introduttiva:

### Deployment one-click su Hasura Cloud

Il metodo più facile e veloce per provare Hasura è via [Hasura Cloud](https://hasura.io/docs/latest/graphql/cloud/getting-started/index.html).

1. Fare clic sul seguente pulsante per eseguire il deploy del motore GraphQL su Hasura Cloud, incluso l'add-on Postgres o utilizzando un database Postgres esistente:

    [![Deploy to Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/signup)

2. Apri la console di Hasura

   Clicca sul bottone "Launch console" per aprire la console di Hasura.

3. Esegui la tua prima query GraphQL

   Crea una tabella ed esegui subito la tua prima query. Segui questa [semplice guida](https://hasura.io/docs/latest/graphql/core/getting-started/first-graphql-query.html).

### Altre opzioni di deployment one-click

Consulta le istruzioni per le seguenti opzioni di deployment one-click:

| **Fornitore dell'infrastruttura** | **Link one-click** | **Informazioni aggiuntive** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [docs](https://hasura.io/docs/latest/graphql/core/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://hasura.io/docs/latest/graphql/core/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://hasura.io/docs/latest/graphql/core/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [docs](https://hasura.io/docs/latest/graphql/core/guides/deployment/render-one-click.html) |

### Altri metodi di deployment

Per il deployment basato su Docker e opzioni avanzate di configurazione, consulta le 
[guide per il deployment](https://hasura.io/docs/latest/graphql/core/getting-started/index.html) o
i [manifesti d'installazione](install-manifests).

## Architettura

Hasura GraphQL Engine gestisce un'istanza di database Postgres e può accettare richieste GraphQL dalle tue app client. Può essere configurato per funzionare con il tuo sistema di autenticazione già esistente e può gestire il controllo degli accessi usando regole a livello di campo con variabili dinamiche del tuo sistema di autenticazione.

È inoltre possibile unire schemi GraphQL remoti e fornire un'API GraphQL unificata.

![Hasura GraphQL Engine architecture](../assets/hasura-arch.svg)

## Strumenti client-side

Hasura funziona con qualsiasi client GraphQL. Consulta [awesome-graphql](https://github.com/chentsulin/awesome-graphql) per un elenco di client. La nostra [serie di tutorial frontend](https://hasura.io/learn/#frontend-tutorial) ha anche integrazioni con i client GraphQL per diversi framework.

## Aggiungere la business logic

GraphQL Engine fornisce metodi semplici, scalabili e performanti per aggiungere business logic personalizzate al tuo back-end:

### Schemi remoti

Aggiungi risolutori personalizzati in uno schema remoto in aggiunta allo schema GraphQL basato su database di Hasura. Ideale per casi d'uso come l'implementazione di un'API di pagamento o l'esecuzione di query su dati non presenti nel tuo database - [leggi di più](remote-schemas.md).

### Azioni

Le azioni sono un modo per estendere lo schema di Hasura con una business logic personalizzata utilizzando query e mutazioni ad hoc. È possibile aggiungere azioni ad Hasura per gestire vari casi d'uso come la convalida dei dati, l'arricchimento dei dati da fonti esterne e qualsiasi altra business logic complessa - [leggi di più](https://hasura.io/docs/latest/graphql/core/actions/index.html)

### Attiva webhook su eventi del database

Aggiungi una business logic asincrona attivabile in base agli eventi del database.
Ideale per notifiche, pipeline di dati da Postgres o processing asincrono - [leggi di più](event-triggers.md).

### Dati derivati o trasformazioni di dati

Trasforma i dati in Postgres o esegui la business logic su di essi per derivare un altro set di dati che può essere interrogato utilizzando GraphQL Engine - [leggi di più](https://hasura.io/docs/latest/graphql/core/queries/derived-data.html).

## Demo

Guarda tutte le applicazioni d'esempio nella cartella [hasura/sample-apps](https://github.com/hasura/sample-apps/tree/main).

### Applicazioni in tempo reale

- Applicazione di chat di gruppo realizzata con React. Include un indicatore di digitazione, 
  utenti online e notifiche per i nuovi messaggi.
  - [Prova](https://realtime-chat.demo.hasura.io/)
  - [Tutorial](https://github.com/hasura/sample-apps/tree/main/realtime-chat)

- App di localizzazione in tempo reale che mostra un veicolo in corsa che cambia le 
  coordinate GPS correnti in movimento su una mappa.
  - [Prova](https://realtime-location-tracking.demo.hasura.io/)
  - [Tutorial](https://github.com/hasura/sample-apps/tree/main/realtime-location-tracking)

- Una dashboard in tempo reale per aggregazioni di dati in continua evoluzione.
  - [Prova](https://realtime-poll.demo.hasura.io/)
  - [Tutorial](https://github.com/hasura/sample-apps/tree/main/realtime-poll)

### Video

* [Aggiungere GraphQL a un'istanza GitLab self-hosted](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Todo app con Auth0 e GraphQL backend](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GraphQL su GitLab integrato con GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [Dashboard per 10 millioni di percorsi con geolocalizzazione (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)

## Supporto e Risoluzione dei Problemi

La documentazione e la community ti aiuteranno a risolvere la maggior parte dei problemi. Se hai riscontrato un bug o hai bisogno di metterti in contatto con noi, puoi contattarci utilizzando uno dei seguenti canali:

* Supporto & feedback: [Discord](https://discord.gg/hasura)
* Tracciamento di problemi e bug: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Segui gli aggiornamenti del prodotto: [@HasuraHQ](https://twitter.com/hasurahq)
* Parla con noi sulla [chat del sito](https://hasura.io)

Ci impegniamo a promuovere nella comunity un ambiente aperto e accogliente. Per favore consulta il [Codice di Condotta](code-of-conduct.md).

Se desideri segnalare un problema di sicurezza, per favore [leggi questo](SECURITY.md).

## Resta aggiornato

Rilasciamo nuove features ogni mese. Iscriviti alla nostra newsletter utilizzando il link sottostante. Inviamo le newsletter soltanto una volta al mese.
[https://hasura.io/newsletter/](https://hasura.io/newsletter/)

## Contribuire

Dai un'occhiata alla nostra [guida per i contributi](CONTRIBUTING.md) per ulteriori dettagli.

## Asset del Marchio

Gli asset del marchio Hasura (loghi, la mascotte di Hasura, i badge "powered by", etc.) possono
essere trovati nella cartella [assets/brand](../assets/brand). Sentiti libero di usarle nella tua
applicazione/sito web etc. Saremmo entusiasti se aggiungessi il badge "Powered by Hasura"
alle tue applicazioni create utilizzando Hasura. ❤️

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_primary_darkbg.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_primary_lightbg.svg" width="150px"/>
</div>

```html
<!-- Su sfondi chiari -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_primary_darkbg.svg" />
</a>

<!-- Su sfondi scuri -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_primary_lightbg.svg" />
</a>
```

## Licenza

Il motore di base GraphQL è disponibile sotto la [Licenza Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Tutti gli **altri contenuti** (eccetto quelli nelle cartelle [`server`](server), [`cli`](cli) e
[`console`](console)) sono disponibili sotto la [Licenza MIT](LICENSE-community).
Questo include l'intero contenuto delle cartelle [`docs`](docs) e [`community`](community).

## Traduzioni

Questo readme è disponibile nelle seguenti traduzioni:

- [Japanese :jp:](../translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [French :fr:](../translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Bosnian :bosnia_herzegovina:](../translations/README.bosnian.md) (:pray: [@hajro92](https://github.com/hajro92))
- [Russian :ru:](../translations/README.russian.md) (:pray: [@highflyer910](https://github.com/highflyer910))
- [Greek 🇬🇷](../translations/README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))
- [Spanish 🇲🇽](../translations/README.mx_spanish.md)(:pray: [@ferdox2](https://github.com/ferdox2))
- [Indonesian :indonesia:](../translations/README.indonesian.md) (:pray: [@anwari666](https://github.com/anwari666))
- [Brazilian Portuguese :brazil:](../translations/README.portuguese_br.md) (:pray: [@rubensmp](https://github.com/rubensmp))
- [German 🇩🇪](../translations/README.german.md) (:pray: [@FynnGrandke](https://github.com/FynnGrandke))
- [Chinese :cn:](../translations/README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))
- [Turkish :tr:](../translations/README.turkish.md) (:pray: [@berat](https://github.com/berat))
- [Korean :kr:](../translations/README.korean.md) (:pray: [@라스크](https://github.com/laskdjlaskdj12))
- [Italian :it:](../translations/README.italian.md) (:pray: [@befire](https://github.com/francesca-belfiore))

Le traduzioni di altri file possono essere trovate [qui](translations).
