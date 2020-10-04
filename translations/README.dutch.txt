# Hasura GraphQL Engine

[! [Laatste release] (https://img.shields.io/github/v/release/hasura/graphql-engine)] (https://github.com/hasura/graphql-engine/releases/latest)
[! [Documenten] (https://img.shields.io/badge/docs-v1.x-brightgreen.svg?style=flat)] (https://hasura.io/docs)
[! [CircleCI] (https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)] (https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine is een razendsnelle GraphQL-server die u **instant, realtime GraphQL API's over Postgres**, met [**webhook triggers**](event-triggers.md) op databasegebeurtenissen en [**remote schema's**](remote-schemas.md) voor bedrijfslogica geeft.

Hasura helpt u bij het bouwen van [GraphQL](https://hasura.io/graphql/) apps ondersteund door Postgres of stapsgewijs naar GraphQL gaan voor bestaande toepassingen met Postgres.

Lees meer op [hasura.io](https://hasura.io) en de [docs](https://hasura.io/docs/).

------------------

! [Hasura GraphQL Engine Demo] (assets/demo.gif)

------------------

! [Hasura GraphQL Engine Realtime Demo] (assets/realtime.gif)

-------------------

## Functies

* **Maak krachtige query's**: Ingebouwde filtering, pagination, patroonzoeken, bulkinvoegen, bijwerken, mutaties verwijderen
* **Realtime**: Converteer elke GraphQL-query naar een live query met abonnementen
* **Externe schema's samenvoegen**: toegang tot aangepaste GraphQL-schema's voor bedrijfslogica via één GraphQL-engineeindpunt. [**Lees meer**] (remote-schemas.md).
* **Trigger webhooks of serverless functions**: Op Postgres invoegen/update/delete gebeurtenissen ([lees meer](event-triggers.md))
* **Werkt met bestaande, live databases**: Wijs het aan een bestaande Postgres-database om direct een kant-en-klare GraphQL API te krijgen
* **Fijnkorrelige toegangscontrole**: Dynamische toegangscontrole die integreert met uw auth-systeem (bijv. auth0, firebase-auth)
* **High-performance & low-footprint**: ~15MB docker image; ~ 50MB RAM @ 1000 req /s; multi-core bewust
* **Admin UI & Migrations**: Admin UI & Rails-geïnspireerde schemamigraties
* **Postgres** ❤️: Ondersteunt Postgres-typen (PostGIS/geo-locatie, enz.), verandert weergaven in *grafieken*, activeer opgeslagen functies of procedures met mutaties

Lees meer op [hasura.io](https://hasura.io) en de [docs](https://hasura.io/docs/).

## Inhoudsopgave
<!-- markdown-toc start - Bewerk deze sectie niet. M-x markdown-toc-refresh-toc uitvoeren -->
**Inhoudsopgave**

- [Snelstart:](#quickstart)
    - [Implementatie met één klik op Hasura Cloud](#one-click-deployment-on-hasura-cloud)
    - [Andere implementatieopties met één klik](#other-one-click-deployment-options)
    - [Andere implementatiemethoden](#other-implementatiemethoden)
- [Architectuur](#architecture)
- [Tooling aan de zijkant van de klant](#client-side-tooling)
- [Bedrijfslogica toevoegen](#add-business-logica)
    - [Externe schema's](#remote-schema's)
    - [Webhooks triggeren op databasegebeurtenissen](#trigger-webhooks-on-database-events)
- [Demo's](#demos)
    - [Realtime-aanvragen](#realtime-toepassingen)
    - [Video's](#videos)
- [Ondersteuning & Probleemoplossing](#support--probleemoplossing)
- [Bijdragen](#contributing)
- [Merkactiva](#brand-assets)
- [Licentie](#license)
- [Vertalingen](#translations)


<!-- markdown-toc-eind -->

## Snelstart:

### Implementatie met één klik op Hasura Cloud

De snelste en eenvoudigste manier om Hasura uit te proberen is via [Hasura Cloud](https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html).

1. Klik op de volgende knop om GraphQL-engine te implementeren op Hasura Cloud, inclusief Postgres-add-on of met behulp van een bestaande Postgres-database:

[! [Implementeren naar Hasura Cloud] (https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)] (https://cloud.hasura.io/)

2. Open de Hasura-console

Klik op de knop "Startconsole" om de Hasura-console te openen.

3. Maak uw eerste GraphQL-query

Maak een tabel en voer direct uw eerste query uit. Volg deze [eenvoudige handleiding](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Andere implementatieopties met één klik

Bekijk de instructies voor de volgende implementatieopties met één klik:

| **Infraaanbieder** | **Link met één klik** | **Aanvullende informatie** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [! [Implementeren naar Heroku] (https://www.herokucdn.com/deploy/button.svg)] (https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [documenten] (https://hasura.io/docs/1.0/graphql/manual/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [! [Implementeren naar DigitalOcean] (https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)] (https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [documenten] (https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [! [Implementeren naar Azure] (http://azuredeploy.net/deploybutton.png)] (https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [documenten] (https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |
| Gerenderde diensten | [! [Implementeren om te renderen] (https://render.com/images/deploy-to-render-button.svg)] (https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [documenten] (https://hasura.io/docs/1.0/graphql/manual/guides/deployment/render-one-click.html) |

### Andere implementatiemethoden

Zie [implementatie op basis van Docker- en geavanceerde configuratieopties voor implementatie op basis van Docker en geavanceerde configuratieopties
gidsen](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) of
[installatiemanifesten] (install-manifests).

## Architectuur

De Hasura GraphQL Engine fronts een Postgres database instance en kan GraphQL-verzoeken van uw client-apps accepteren. Het kan worden geconfigureerd om te werken met uw bestaande auth-systeem en kan toegangscontrole aan met behulp van veldniveauregels met dynamische variabelen van uw auth-systeem.

U ook externe GraphQL-schema's samenvoegen en een uniforme GraphQL-API bieden.

! [Hasura GraphQL Engine-architectuur] (activa/hasura-arch.svg)

## Client-side tooling

Hasura werkt met elke GraphQL-client. We raden u aan [Apollo Client](https://github.com/apollographql/apollo-client) te gebruiken. Zie [awesome-graphql](https://github.com/chentsulin/awesome-graphql) voor een lijst met clients.

## Bedrijfslogica toevoegen

GraphQL Engine biedt eenvoudig te redeneren, schaalbare en performante methoden voor het toevoegen van aangepaste bedrijfslogica aan uw backend:

### Externe schema's

Voeg aangepaste resolvers toe in een extern schema naast het Op Postgres gebaseerde GraphQL-schema van Hasura. Ideaal voor use-cases zoals het implementeren van een betaal-API of het opvragen van gegevens die niet in uw database staan - [lees meer](remote-schemas.md).

### Trigger webhooks op database-gebeurtenissen

Voeg asynchrone bedrijfslogica toe die wordt geactiveerd op basis van databasegebeurtenissen.
Ideaal voor meldingen, data-pipelines van Postgres of asynchrone
verwerking - [lees meer](event-triggers.md).

### Afgeleide gegevens of gegevenstransformaties

Transformeer gegevens in Postgres of voer er bedrijfslogica op uit om een andere gegevensset af te leiden die kan worden opgevraagd met Behulp van GraphQL Engine - [lees meer](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## Demo's

Bekijk alle voorbeeldtoepassingen in de map [community/sample-apps](community/sample-apps).

### Realtime-toepassingen

- Group Chat applicatie gebouwd met React, inclusief een typeindicator, online gebruikers & nieuw
  berichtmeldingen.
  - [Probeer het uit](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-chat)
  - [Blader door API's](https://realtime-chat.demo.hasura.app/console)

- Live locatie tracking app die een lopend voertuig verandert huidige GPS toont
  coördinaten die zich op een kaart bewegen.
  - [Probeer het uit](https://realtime-location-tracking.demo.hasura.app/)
  - [Zelfstudie](community/sample-apps/realtime-location-tracking)
  - [Blader door API's](https://realtime-location-tracking.demo.hasura.app/console)

- Een realtime dashboard voor gegevensaggregaties over continu veranderende gegevens.
  - [Probeer het uit](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-poll)
  - [Blader door API's](https://realtime-poll.demo.hasura.app/console)

### Video's

* [Voeg GraphQL toe aan een self-hosted GitLab-exemplaar](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 minuten*)
* [Todo app met Auth0 en GraphQL backend](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 minuten*)
* [GraphQL op GitLab geïntegreerd met GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 minuten*)
* [Dashboard voor 10 miljoen ritten met geo-locatie (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yGWA) (*3:06 mins*)

## Ondersteuning & Probleemoplossing

De documentatie en de community helpen u de meeste problemen op te lossen. Als je een bug hebt tegengekomen of contact met ons moet opnemen, kun je contact met ons opnemen via een van de volgende kanalen:

* Ondersteuning & feedback: [Discord](https://discord.gg/hasura)
* Issue & bug tracking: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Volg productupdates: [@HasuraHQ](https://twitter.com/hasurahq)
* Praat met ons op onze [website chat](https://hasura.io)

Wij zetten ons in voor een open en gastvrije omgeving in de gemeenschap. Zie de [Gedragscode](code-of-conduct.md).

Als u een beveiligingsprobleem wilt melden, u [lees dit](SECURITY.md).

## Bijdragen

Bekijk onze [bijdragende gids](CONTRIBUTING.md) voor meer informatie.

## Merkactiva

Hasura merkactiva (logo's, de Hasura mascotte, aangedreven door badges etc.) kunnen worden
gevonden in de map [assets/brand](assets/brand). Gebruik ze gerust in uw
applicatie/website etc. We zouden blij zijn als je de "Powered by Hasura" toe te voegen
badge voor uw toepassingen gebouwd met Hasura. ❤️

<div style="display: flex;">
  <img src="assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

'''html
<!-- voor lichte achtergronden -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- voor donkere achtergronden -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## Licentie

De kern GraphQL Engine is beschikbaar onder de [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Alle **andere inhoud** (behalve die in ['server'](server), ['cli'](cli) en
['console'] (console) mappen) zijn beschikbaar onder de [MIT License](LICENSE-community).
Dit omvat alles in de ['docs'](docs) en ['community'](community)
Mappen.

## Vertalingen

Deze readme is beschikbaar in de volgende vertalingen:

- [Japans :jp:](vertalingen/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [Frans :fr:](vertalingen/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Bosnisch :bosnia_herzegovina:](vertalingen/README.bosnian.md) (:pray: [@hajro92](https://github.com/hajro92))
- [Russisch :ru:](vertalingen/README.russian.md) (:pray: [@highflyer910](https://github.com/highflyer910))
- [Griekse 🇬🇷](vertalingen/README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))
- [Spaanse 🇲🇽](/vertalingen/README.mx_spanish.md)(:pray: [@ferdox2](https://github.com/ferdox2))
- [Indonesisch :indonesië:](vertalingen/README.indonesian.md) (:pray: [@anwari666](https://github.com/anwari666))
- [Braziliaans Portugees :brazilië:](vertalingen/README.portuguese_br.md) (:pray: [@rubensmp](https://github.com/rubensmp))
- [Duitse 🇩🇪](vertalingen/README.german.md) (:pray: [@FynnGrandke](https://github.com/FynnGrandke))
- [Chinees :cn:](vertalingen/README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))
- [Turks :tr:](vertalingen/README.turkish.md) (:pray: [@berat](https://github.com/berat))
- [Koreaans :kr:](vertalingen/README.korean.md) (:pray: [@ー크](https://github.com/laskdjlaskdj12))

Vertalingen voor andere bestanden zijn te vinden [hier](vertalingen).
