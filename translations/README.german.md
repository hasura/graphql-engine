# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)

<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine ist ein blitzschneller GraphQL Server, welcher dir die Möglichkeit bietet, **sofort und in Echtzeit GraphQL APIs in Postgres** zu entwickeln, die mit [**webhook triggern**](../event-triggers.md) für Datenbank-Events und [**remote schematas**](../remote-schemas.md) für Businesslogik erweitert werden können.

Hasura hilft dir, GraphQL-Anwendungen basierend auf Postgres zu entwickeln oder schrittweise zu GraphQL zu wechseln für schon existierende Anwendungen, die Postgres benutzen.

Dazu mehr auf [hasura.io](https://hasura.io) und in den [Docs](https://hasura.io/docs).

---

![Hasura GraphQL Engine Demo](../assets/demo.gif)

---

![Hasura GraphQL Engine Realtime Demo](../assets/realtime.gif)

---

## Features

- **Entwickle mächtige Queries**: Built-in Filter, Paginierung, Suchmuster, Bulk-Inserts, Update- und Delete-Mutationen
- **Echtzeit**: Konvertiere jede GraphQL Query zu einer Live-Query mit Hilfe von Subscriptions
- **Merge Remote Schemas**: Zugriff auf individuelle GraphQL Schemas für Businesslogik mit einem einzigen GraphQL Engine-Endpoint. [Mehr dazu](../remote-schemas.md).
- **Steuere Webhooks oder Severless-Funktionen**: Mit Postgres insert/update/delete Events. [Mehr dazu](../event-triggers.md).
- **Funktioniert mit bereits existierenden Live-Datenbanken**: Steuere eine bereits existierende Postgres Datenbank an, um eine fertige GraphQL API zu erhalten
- **Detailierte Zugriffssteuerung**: Dynamischer Zugriff, der sich mühelos mit deinem Authentifizierungssystem verbinden lässt (z.B.: auth0, firebase-auth)
- **Hochperformant & wenig Speicherbedarf**: ~15MB docker image; ~50MB RAM @ 1000 req/s; multi-core aware
- **Admin UI & Migration**: Admin UI & von Rails inspirierte Schemata-Integration
- **Postgres** ❤️: Unterstützt Postgres Typen (PostGIS/geo-location, etc.), ändert Views zu _Graphen_, löst gespeicherte Funktionen aus oder Prozesse mit Mutationen

Mehr dazu auf [hasura.io](https://hasura.io) und in den [Docs](https://hasura.io/docs).

## Inhaltsverzeichnis

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Schnellstart](#schnellstart)
  - [One-click Deployment auf Heroku](#one-click-deployment-auf-heroku)
  - [Andere one-click Deployment Optionen](#andere-one-click-deployment-optionen)
- [Architektur](#architektur)
- [Client-side Tooling](#client-side-tooling)
- [Füge Businesslogik hinzu](#füge-businesslogik-hinzu)
  - [Remote Schemas](#remote-schemas)
  - [Löse Webhooks und Datenbankevents aus](#löse-webhooks-und-datenbankevents-aus)
- [Demos](#demos)
  - [Realtime Applikationen](#realtime-applikationen)
  - [Videos](#videos)
- [Support & Fehlerbehebung](#support--fehlerbehebung)
- [Contributing](#contributing)
- [Marke](#marke)
- [Lizenz](#lizenz)
- [Übersetzungen](#uebersetzungen)

<!-- markdown-toc end -->

## Schnellstart:

### One-click Deployment auf Heroku

Der schnellste Weg, Hasura auszuprobieren, ist mit Heroku.

1. Klick auf den folgenden Button, um mit dem kostenlosen Postgress Add-On die GraphQL Engine auf Heroku zu deployen:

   [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Öffne die Hasura Console

   Besuche `https://<app-name>.herokuapp.com` (_ersetze \<app-name\> mit dem Namen deiner App_), um die Admin Console zu öffnen.

3. Erstelle deine erste GraphQL Query

   Erstelle eine Tabelle und lasse deine erste Query laufen. Folge diesem [einfachem Guide](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Andere one-click Deployment Optionen

Probiere die folgenden Guides für die one-click Deployment Optionen:

| **Infra Anbieter** |                                                                                                                         **One-click Link**                                                                                                                         |                                                            **Zusätzliche Information**                                                            |
| :----------------: | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: | :-----------------------------------------------------------------------------------------------------------------------------------------------: |
|    DigitalOcean    |                  [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme)                  | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
|       Azure        | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) |                    [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html)                    |

### Andere Deployment Möglichkeiten

Für auf Docker basierende Deployments and erweiterte Konfigurationsmöglichkeiten: [Deployment
Guides](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) oder
[Manifest installieren](../install-manifests).

## Architektur

Die Hasura GraphQL Engine simuliert eine Postgres Datenbank-Instanz and kann Anfragen von Client-Apps verarbeiten. Sie kann so konfiguriert werden, dass es mit deinem schon bestehenden Authentifizierungssystem funktioniert und kann dementsprechend Zugriffe via "field-level"-Regeln mit dynamischen Variablen von deinem Autehentifizierungssystem kontrollieren.

Du kannst auch Remote-GraphQL Schemas einbinden und so eine vereinheitlichte GraphQL API benutzen.

![Hasura GraphQL Engine Architektur](../assets/hasura-arch.svg)

## Client-side Tooling

Hasura funktioniert mit jedem GraphQL Client. Wir empfehlen [Apollo Client](https://github.com/apollographql/apollo-client). Andernfalls besuche [awesome-graphql](https://github.com/chentsulin/awesome-graphql) für eine Liste von anderen möglichen Clients.

## Füge Businesslogik hinzu

Die GraphQL Engine bietet einfach zu verstehende, skalierende und performante Möglichkeiten, um deinem Backend individuelle Businesslogik hinzuzufügen.

### Remote Schemas

Füge zusätzlich zu Hasuras Postgres-basierendem GraphQL Schema deine individuellen Resolver in einem Remote Schema hinzu. Dies hilft zum Beispiel bei der Implementierung von einer Bezahl-API oder beim Anfragen von Daten, die nicht in deiner eigenen Datenbank sind - [mehr dazu](../remote-schemas.md).

### Löse Webhooks und Datenbankevents aus

Füge asynchrone Businesslogik hinzu, welche von Datenbankevents ausgelöst wird.
Das ist perfekt für Benachrichtigungen, Daten-Pipelines von Postgres oder asynchrones Processing - [mehr dazu](../event-triggers.md).

### Abgeleitete Daten oder Datentransformation

Transformiere Daten in Postgres oder lasse deine Businesslogik eigene Daten ableiten, welche von der GraphQL Engine abefragt werden können - [mehr dazu](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## Demos

Schaue dir die Beispiele in dem
[community/sample-apps](../community/sample-apps) Verzeichnis an.

### Realtime Applikationen

- Gruppenchat entwickelt mit React, beinhaltet einen Typenanzeiger, Online Users und Benachrichtigungen bei
  neuen Nachrichten.

  - [Probiere es aus](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-chat)
  - [Durchsuche die APIs](https://realtime-chat.demo.hasura.app/console)

- Echtzeit Standort Tracking App, welche die Koordinaten von Fahrzeugen
  verfolgt und auf einer Karte anzeigt.

  - [Probiere es aus](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-location-tracking)
  - [Durchsuche die APIs](https://realtime-location-tracking.demo.hasura.app/console)

- Echtzeit Dashboard für Datenaggregation von sich konstant ändernden Daten.
  - [Probiere es aus](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-poll)
  - [Durchsuche die APIs](https://realtime-poll.demo.hasura.app/console)

### Videos

- [Füge GraphQL zu einer eigenen GitLab Instanz hinzu](https://www.youtube.com/watch?v=a2AhxKqd82Q) (_3:44 mins_)
- [Todo App mit Auth0 und GraphQL Backend](https://www.youtube.com/watch?v=15ITBYnccgc) (_4:00 mins_)
- [GraphQL auf GitLab mit GitLab Auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (_4:05 mins_)
- [Dashboard für 10 mio. Fahrzeuge mit Geo-Koordinaten (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (_3:06 mins_)

## Support & Fehlerbehebung

Die Dokumentation und die Community hilft bei der Fehlersuche und -behebung. Wenn du einen Bug gefunden hast oder mit uns in Verbindung treten möchtest, kannst du uns folgendermassen kontaktieren:

- Support & Feedback: [Discord](https://discord.gg/vBPpJkS)
- Issue & Bug Tracking: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
- Folge unseren Updates: [@HasuraHQ](https://twitter.com/hasurahq)
- Schreibe uns im [Webseiten Chat](https://hasura.io)

Wir sind stets bemüht um einen gepflegten und freundlichen Umgang in der Community. Bitte schaue dir dazu den [Code of Conduct](../code-of-conduct.md) an.

Wenn du ein Sicherheitsrisiko melden möchtest, bitte [lese dies](../SECURITY.md).

## Contributing

Lese hier den [Contributing Guide](../CONTRIBUTING.md) für mehr Details.

## Marke

Die Marke Hasura (logos, das Hasura Maskottchen, Abzeichen, etc.) kannst du
hier finden [../assets/brand](../assets/brand). Du kannst sie in deiner App,
Webseite, etc. benutzen. Wir würden uns sehr freuen, wenn du das "Powered by Hasura"
Abzeichen deiner Hasura App hinzufügst. ❤️

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

## Lizenz

Die Kern-GraphQL Engine ist verfügbar unter [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Alle **anderen Inhalte** (ausser die in [`server`](../server), [`cli`](../cli) und
[`console`](../console) Verzeichnissen) sind verfügbar unter der [MIT Lizenz](../LICENSE-community).
Dies beinhaltet [`docs`](../docs) und [`community`](../community)
Verzeichnisse.

## Übersetzungen

Dieses Readme ist under anderem in den folgenden Sprachen verfügbar:

- [Japanese :jp:](README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [French :fr:](README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Greek 🇬🇷](README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))

Übersetzungen von anderen Dateien kannst du [hier](../translations) finden.
