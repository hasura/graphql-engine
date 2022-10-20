# Hasura GraphQL Engine

[![Latest release](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![Docs](https://img.shields.io/badge/docs-v1.x-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasuras GraphQL-motor er en lynrask GraphQL-server som gir deg et **øyeblikkelig, sanntids GraphQL-API som bruker Postgres i bunn**. I løsningen finner du _[**webhook-triggere**](../event-triggers.md)_, som kan trigges ved endringer i databasen, samt muligheter for [**eksterne skjemaer**](../remote-schemas.md) for å skille ut forretningslogikken din.

Hasura hjelper deg å lage et [GraphQL](https://hasura.io/graphql/)-grensesnitt for applikasjoner som bruker Postgres i bunn, eller kan fungere som et lag mellom eksisterende Postgres-database for å smidigere fase over til GraphQL.

Les mer på [hasura.io](https://hasura.io) og i [dokumentasjonen](https://hasura.io/docs/).

------------------

![Hasura GraphQL Engine Demo](../assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](../assets/realtime.gif)

------------------

## Funksjoner

* **Kraftige spørringer**: Innebygd filtrering, mulighet for sideoppdeling, søk, masseopprettelse, -oppdatering og -sletting
* **Sanntid**: Konverter en hvilken som helst spørring til sanntidsdata ved å abonnere på endringer
* **Sammenslåtte skjema**: Få tilgang på egne GraphQL-skjema for forretningslogikk ved hjelp av ett endepunkt. [**Les mer**](remote-schemas.md)
* **Trigge webhooks eller serverløse funksjoner**: Ved Postgres insert/update/delete-endringer. [Les mer](event-triggers.md)
* **Fungerer med eksisterende databaser**: Pek løsningen til en eksisterende Postgres database for å komme i gang
* **Findelt tilgangsstyring**: Dynamisk tilgangsstyring som lett integreres mot eksisterende autentiseringsløsning (eks: auth0, firebase-auth)
* **Høy ytelse og lite ressurskrevende**: ~15MB docker image; ~50MB RAM @ 1000 req/s; støtter flere kjerner
* **Admin UI og migrering**: Admin UI og Rails-inspirert schema-migrering
* **Postgres** ❤️: Støtter Postgres typer (PostGIS/geo-location, osv.), konverter *views* til *graphs*, og lar deg trigge lagrede Postgres-funksjoner/prosedyrer med *GraphQL mutations*

Les mer på [hasura.io](https://hasura.io) og i [dokumentasjonen](https://hasura.io/docs/).

## Innholdsfortegnelse
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Kom i gang:](#kom-i-gang)
    - [Ett klikk-deployment til Hasura Cloud](#ett-klikk-deployment-til-hasura-cloud)
    - [Andre ett klikk-alternativer](#andre-ett-klikk-alternativer)
    - [Andre metoder](#andre-metoder)
- [Arkitektur](#arkitektur)
- [Klientapplikasjoner](#klientapplikasjoner)
- [Forretningslogikk](#forretningslogikk)
    - [Remote schemas](#remote-schemas)
    - [Webhooks-utløsing ved endringer i databasen](#webhooks-utløsing-ved-endringer-i-databasen)
- [Demonstrasjoner](#demonstrasjoner)
    - [Sanntidsapplikasjoner](#sanntidsapplikasjoner)
    - [Videoer](#videoer)
- [Støtte og feilsøking](#støtte-og-feilsøking)
- [Bidra i prosjektet!](#bidra-i-prosjektet!)
- [Merkevare](#merkevare)
- [Lisens](#lisens)
- [Oversettelser](#oversettelser)

<!-- markdown-toc end -->

## Kom i gang

### Ett klikk-deployment til Hasura Cloud

Den kjappeste og enkleste måten å prøve ut Hasura på, er via [Hasura Cloud](https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html).

1. Klikk på knappen nedenfor for å sette opp GraphQL-motoren vår i Hasura Cloud. Her kan du også få en inkludert Postgres-database, eller du kan velge å bruke din eksisterende database:

    [![Deploy i Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

2. Åpne Hasura-konsollen

   Klikk på "Launch console"-knappen for å åpne Hasura-konsollen

3. Kjør din første GraphQL-spørring

   Lag en tabell og kjør din første spørring! Følg [denne enkle guiden](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html) for å komme i gang.

### Andre ett klikk-alternativer

Sjekk ut instruksjonene nedenfor, for å sette opp Hasura hos disse leverandørene:

| **Leverandør** | **Ett klikk-deployment** | **Annen informasjon** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [Dokumentasjon](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [Dokumentasjon](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [Dokumentasjon](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [Dokumentasjon](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/render-one-click.html) |

### Andre metoder

For Docker-baserte miljøer, og andre avanserte konfigurasjoner, se [deployment
guides](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html)-siden eller
[installasjonsmanifestene](../install-manifests).

## Arkitektur

Hasura GraphQL-motoren står foran Postgres-databaser, og kan ta imot GraphQL-forespørsler fra applikasjonene dine. Den kan konfigureres til å virke med din eksisterende autentiseringsløsning, og kan håndtere tilgangsstyring på feltnivå med dynamiske innstillinger fra autentiseringsløsningen din.

Du kan også slå sammen flere eksterne GraphQL-skjemaer og tilby ett enhetlig GraphQL-API med data fra flere kilder.

![Hasura GraphQL Engine-arkiterktur](../assets/hasura-arch.svg)

## Klientapplikasjoner

Hasura fungerer med hvilken som helst GraphQL-klient. Vi anbefaler å bruke [Apollo's GraphQL-klient](https://github.com/apollographql/apollo-client), men ta gjerne en titt på [awesome-graphql](https://github.com/chentsulin/awesome-graphql)-lista for en oversikt over hvilke klienter som fungerer bra!

## Forretningslogikk

Motoren gir deg en skalerbar, rask og effektiv funksjonalitet for å legge til egen forretningslogikk i backenden:

### Remote schemas

Legg til *custom resolvers* for et å implementere eksternt skjema i tillegg til Hasuras stadnard Postgres-skjema. Ideelt for tilfeller der man feks vil implementere en betalings-API, eller for å spørre etter data som ikke er tilgjengelig i databasen - [les mer](../remote-schemas.md).

### Webhooks-utløsing ved endringer i databasen

Kjør asynkron forretningslogikk ved endringer i databasen.
Ideelt for varsler, datapipeline fra Postgres eller asynkron prosessering av data - [ler mer](../event-triggers.md).

### Endrede og aggregerte datasett

Transformér data med Postgres-spørringer, eller kjør egen logikk for å endre dataene før de returneres via GraphQL-motoren - [ler mer](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## Demonstrasjoner

Sjekk ut alle eksempelene i [community/sample-apps](../community/sample-apps)-mappa.

### Sanntidsapplikasjoner

- Gruppechat utviklet med React. Inkluderer "skriveindikator", oversikt over brukere og varsler ved nye meldinger
  - [Prøv meg](https://realtime-chat.demo.hasura.app/)
  - [Veiledning](community/sample-apps/realtime-chat)
  - [Utforsk API-ene](https://realtime-chat.demo.hasura.app/console)

- Sporingsapp som viser et kjøretøys nøyaktige posisjon ved hjelp av GPS-koorinater, illustrert på et kart.  
  - [Prøv meg](https://realtime-location-tracking.demo.hasura.app/)
  - [Veiledning](community/sample-apps/realtime-location-tracking)
  - [Utforsk API-ene](https://realtime-location-tracking.demo.hasura.app/console)

- Sanntidsdashbord for visning av aggregerte data på direkten.
  - [Prøv meg](https://realtime-poll.demo.hasura.app/)
  - [Veiledning](community/sample-apps/realtime-poll)
  - [Utforsk API-ene](https://realtime-poll.demo.hasura.app/console)

### Videoer

* [Add GraphQL to a self-hosted GitLab instance](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 min*)
* [Todo app with Auth0 and GraphQL backend](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 min*)
* [GraphQL on GitLab integrated with GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 min*)
* [Dashboard for 10million rides with geo-location (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 min*)

## Støtte og feilsøking

Dokumentasjonen og fellesskapet vil hjelpe deg med feilsøking av de fleste problemer du skulle komme over. Om du finner en bug, eller trenger å komme i kontakt med oss, kan du kontakte oss på følgende steder:

* Tilbakemeldinger og bistand: [Discord](https://discord.gg/hasura)
* Rapportering av feil/bugs: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Oppdateringer: [@HasuraHQ](https://twitter.com/hasurahq)
* Chat med oss på [nettsiden vår](https://hasura.io)

Vi er forpliktet til å fremme et åpent miljø hvor alle trives, sånn blir det trivligst for alle! Sett deg gjerne inn i vår [Code of Conduct](../code-of-conduct.md), så er vi sikre på at vi er på samme side!

Ønsker du å rapportere eller diskutere et mulig sikkerhetshull, [les dette først](../SECURITY.md).

## Bidra i prosjektet!

Sjekk gjerne ut vår [veiledning for bidragsytere](../CONTRIBUTING.md) for flere detaljer om hvordan du kan bidra til å gjøre løsningen vår enda bedre!

## Merkevare

Hasuras merkevare (logo, maskot, "powered by"-merker osv.) kan du finne i [assets/brand](../assets/brand)-mappa.
Du står fritt til å bruke dette i applikasjonen/på nettsiden din, og vi hadde satt skikkelig pris på om du la til et *"Powered by Hasura"*-merke i appen din! ❤️

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- For lyse bakgrunner -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- For mørke bakgrunner -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## Lisens

Kjernemotoren vår er tilgjengelig under [Apache 2.0-lisensen](https://www.apache.org/licenses/LICENSE-2.0).

**Alt annet** innhold (foruten det i [`server`](server), [`cli`](cli) eller
[`console`](console)-mappene) er tilgjengelig under [MIT-lisensen](../LICENSE-community).
Dette inkluderer også alt i [`docs`](docs) of [`community`](community)-mappa!

## Oversettelser

Denne siden er også tilgjengelig på disse språkene, takket være våre uvurderlige oversettere!


- [Japanese :jp:](./README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [French :fr:](./README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Bosnian :bosnia_herzegovina:](./README.bosnian.md) (:pray: [@hajro92](https://github.com/hajro92))
- [Russian :ru:](./README.russian.md) (:pray: [@highflyer910](https://github.com/highflyer910))
- [Greek 🇬🇷](./README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))
- [Spanish 🇲🇽](./README.mx_spanish.md)(:pray: [@ferdox2](https://github.com/ferdox2))
- [Indonesian :indonesia:](./README.indonesian.md) (:pray: [@anwari666](https://github.com/anwari666))
- [Brazilian Portuguese :brazil:](./README.portuguese_br.md) (:pray: [@rubensmp](https://github.com/rubensmp))
- [German 🇩🇪](./README.german.md) (:pray: [@FynnGrandke](https://github.com/FynnGrandke))
- [Chinese :cn:](./README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))
- [Turkish :tr:](./README.turkish.md) (:pray: [@berat](https://github.com/berat))
- [Korean :kr:](./README.korean.md) (:pray: [@라스크](https://github.com/laskdjlaskdj12))
- [Norwegian :norway:](./README.norwegian.md) (:pray: [@MatsAnd](https://github.com/MatsAnd))

Oversettelser av de andre sidene finner du [her](../translations).
