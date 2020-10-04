Hasura GraphQL-engine
Laatste release Docs CircleCI



Hasura GraphQL Engine is een razendsnelle GraphQL-server die u directe, realtime GraphQL API's geeft over Postgres, met webhook-triggers op databasegebeurtenissen en externe schema's voor bedrijfslogica.

Hasura helpt u graphql-apps te bouwen die door Postgres worden ondersteund of stapsgewijs naar GraphQL te verhuizen voor bestaande toepassingen met Postgres.

Lees meer bij hasura.io en de documenten.

Demo van Hasura GraphQL-engine

Hasura GraphQL Engine Realtime Demo

Functies
Krachtige query's maken: ingebouwde filtering, pagination, patroonzoeken, bulkinvoegen, bijwerken, mutaties verwijderen
Realtime: Elke GraphQL-query converteren naar een live query met abonnementen
Externe schema's samenvoegen: toegang tot aangepaste GraphQL-schema's voor bedrijfslogica via één GraphQL Engine-eindpunt. Lees meer.
Webhooks of serverloze functies activeren: op Postgres-invoegen/bijwerken/verwijderen gebeurtenissen (lees meer)
Werkt met bestaande, live databases: wijs het aan een bestaande Postgres-database om direct een kant-en-klare GraphQL API te krijgen
Fijnmazige toegangscontrole: dynamische toegangscontrole die integreert met uw auth-systeem (bijv. auth0, firebase-auth)
High-performance & low-footprint: ~15MB docker image; ~ 50MB RAM @ 1000 req /s; multi-core bewust
Admin UI & Migrations: Admin UI & Rails-geïnspireerde schemamigraties
Postgres ❤️: Ondersteunt Postgres-typen (PostGIS/geolocatie, enz.), richt weergaven op grafieken, activeert opgeslagen functies of procedures met mutaties
Lees meer bij hasura.io en de documenten.

Inhoudsopgave
Inhoudsopgave

Quickstart:
Implementatie met één klik op Hasura Cloud
Andere implementatieopties met één klik
Andere implementatiemethoden
Architectuur
Tooling aan clientzijde
Bedrijfslogica toevoegen
Externe schema's
Webhaken activeren bij databasegebeurtenissen
Demos
Realtime-toepassingen
Video 's
Ondersteuning & probleemoplossing
Bijdragen
Merkactiva
Licentie
Translations
Quickstart:
Implementatie met één klik op Hasura Cloud
De snelste en eenvoudigste manier om Hasura uit te proberen is via Hasura Cloud.

Klik op de volgende knop om GraphQL-engine op Hasura Cloud te implementeren, inclusief Postgres-add-on of met behulp van een bestaande Postgres-database:

Implementeren naar Hasura Cloud

De Hasura-console openen

Klik op de knop "Startconsole" om de Hasura-console te openen.

Uw eerste GraphQL-query maken

Maak een tabel en voer direct uw eerste query uit. Volg deze eenvoudige gids.

Andere implementatieopties met één klik
Bekijk de instructies voor de volgende implementatieopties met één klik:

Infraprovider One-click link Aanvullende informatie
Heroku Implementeren naar Heroku docs
DigitalOcean Deploy to DigitalOcean docs DigitalOcean
Azure Deploy naar Azure-documenten
Render deploy to Render-documenten
Andere implementatiemethoden
Zie implementatiehandleidingen of installatiemanifesten voor implementatie- en geavanceerde configuratieopties op basis van Docker.

Architectuur
De Hasura GraphQL Engine fronts een Postgres database instance en kan GraphQL-verzoeken van uw client-apps accepteren. Het kan worden geconfigureerd om te werken met uw bestaande auth-systeem en kan toegangscontrole aan met behulp van veldniveauregels met dynamische variabelen van uw auth-systeem.

U ook externe GraphQL-schema's samenvoegen en een uniforme GraphQL-API bieden.

Hasura GraphQL Engine-architectuur

Tooling aan clientzijde
Hasura werkt met elke GraphQL-client. Wij raden u aan apollo-client te gebruiken. Zie awesome-graphql voor een lijst van klanten.

Bedrijfslogica toevoegen
GraphQL Engine biedt eenvoudig te redeneren, schaalbare en performante methoden voor het toevoegen van aangepaste bedrijfslogica aan uw backend:

Externe schema's
Voeg aangepaste resolvers toe in een extern schema naast het Op Postgres gebaseerde GraphQL-schema van Hasura. Ideaal voor use-cases zoals het implementeren van een betaal-API of het opvragen van gegevens die niet in uw database staan - lees meer.

Webhaken activeren bij databasegebeurtenissen
Voeg asynchrone bedrijfslogica toe die wordt geactiveerd op basis van databasegebeurtenissen. Ideaal voor meldingen, data-pipelines van Postgres of asynchrone verwerking - lees meer.

Afgeleide gegevens of gegevenstransformaties
Transformeer gegevens in Postgres of voer er bedrijfslogica op uit om een andere gegevensset af te leiden die kan worden opgevraagd met Behulp van GraphQL Engine - lees meer.

Demos
Bekijk alle voorbeeldtoepassingen in de directory community/sample-apps.

Realtime-toepassingen
Group Chat applicatie gebouwd met React, bevat een typeindicator, online gebruikers & nieuwe berichtmeldingen.

Probeer het uit
Tutorial
Door API's bladeren
Live locatie tracking app die een lopend voertuig verandert huidige GPS-coördinaten verplaatsen op een kaart toont.

Probeer het uit
Tutorial
Door API's bladeren
Een realtime dashboard voor gegevensaggregaties over continu veranderende gegevens.

Probeer het uit
Tutorial
Door API's bladeren
Video 's
GraphQL toevoegen aan een zelf gehoste GitLab-instantie (3:44 minuten)
Todo app met Auth0 en GraphQL backend (4:00 minuten)
GraphQL op GitLab geïntegreerd met GitLab auth (4:05 minuten)
Dashboard voor 10 miljoen ritten met geolocatie (PostGIS, Tijdschaal) (3:06 minuten)
Ondersteuning & probleemoplossing
De documentatie en de community helpen u de meeste problemen op te lossen. Als je een bug hebt tegengekomen of contact met ons moet opnemen, kun je contact met ons opnemen via een van de volgende kanalen:

Ondersteuning & feedback: Discord
Issue & bug tracking: GitHub issues
Productupdates volgen: @HasuraHQ
Praat met ons op onze website chat
Wij zetten ons in voor een open en gastvrije omgeving in de gemeenschap. Zie de gedragscode.

Als u een beveiligingsprobleem wilt melden, leest u dit.

Bijdragen
Bekijk onze bijdragende gids voor meer informatie.

Merkactiva
Hasura merkactiva (logo's, de Hasura mascotte, aangedreven door badges etc.) is te vinden in de assets/brand map. Voel je vrij om ze te gebruiken in uw applicatie / website etc. We zouden blij zijn als u de "Powered by Hasura" badge toevoegt aan uw toepassingen die zijn gebouwd met Hasura. ❤️

<!-- voor lichte achtergronden -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- voor donkere achtergronden -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
Licentie
De core GraphQL Engine is beschikbaar onder de Apache License 2.0 (Apache-2.0).

Alle andere inhoud (behalve die in server-, cli- en consolemay's) is beschikbaar onder de MIT-licentie. Dit omvat alles in de documenten en community mappen.

Translations
Deze readme is beschikbaar in de volgende vertalingen:

Japanse 🇯🇵 (🙏 @moksahero)
Franse 🇫🇷 (🙏 @l0ck3)
Bosnische 🇧🇦 (🙏 @hajro92)
Russische 🇷🇺 (🙏 @highflyer910)
Griekse 🇬🇷 (🙏 @MIP2000)
Spaanse 🇲🇽(🙏 @ferdox2)
Indonesische 🇮🇩 (🙏 @anwari666)
Braziliaanse Portugese 🇧🇷 (🙏 @rubensmp)
Duitse 🇩🇪 (🙏 @FynnGrandke)
Chinese 🇨🇳 (🙏 @jagreetdg & @johnbanq)
Turkse 🇹🇷 (🙏 @berat)
Koreaanse 🇰🇷 (🙏 @크)
Vertalingen voor andere bestanden zijn hier te vinden.