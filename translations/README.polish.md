# Silnik Hasura GraphQL

[![Ostatnie wydanie](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![Dokumentacja](https://img.shields.io/badge/docs-v2.x-brightgreen.svg?style=flat)](https://hasura.io/docs)

<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://hasura.io/newsletter/"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura jest otwartoźródłowym programem, który przyśpiesza tworzenie API aż 10-krotnie, dając ci natychmiastowy dostęp do [GraphQL](https://hasura.io/graphql/) lub REST APIs z wbudowaną autoryzacją do danych.

Przeczytaj więcej na: [hasura.io](https://hasura.io) oraz w [dokumentacji](https://hasura.io/docs/).

------------------

![Demo silnika Hasura GraphQL](assets/demo.gif)

------------------

![Działające na żywo demo silnika Hasura GraphQL](assets/realtime.gif)

-------------------

## Zalety

* **Tworzenie zaawansowanych zapytań**: wbudowane filtrowanie, paginacja, wyszukiwanie wzorców, wstawianie zbiorcze, aktualizacja, usuwanie mutacji
* **Działa z istniejącymi, aktywnymi bazami danych**: wskaż istniejącą bazę danych, aby natychmiast uzyskać gotowy do użycia interfejs API GraphQL
* **Działanie na żywo**: Konwertuj dowolne zapytanie GraphQL na zapytanie na żywo za pomocą subskrypcji
* **Scal zdalne schematy**: Uzyskaj dostęp do niestandardowych schematów GraphQL dla logiki biznesowej za pośrednictwem jednego punktu końcowego GraphQL Engine. [**Przeczytaj więcej**](remote-schemas.md).
* **Rozszerz za pomocą akcji**: Napisz interfejsy API REST, aby rozszerzyć schemat Hasury o niestandardową logikę biznesową.
* **Wyzwalaj webhooki lub funkcje bezserwerowe**: podczas wstawiania/aktualizowania/usuwania zdarzeń w Postgresie, ([czytaj więcej](event-triggers.md))
* **Zaplanowane wyzwalacze**: Wykonywanie niestandardowej logiki biznesowej w określonych momentach za pomocą konfiguracji crona lub jednorazowego zdarzenia.
* **Szczegółowa kontrola dostępu**: dynamiczna kontrola dostępu integrująca się z systemem uwierzytelniania (np.: auth0, firebase-auth)
* **Interfejs administratora i migracje**: migracje schematów inspirowane interfejsem administratora i Rails
* **Obsługiwane bazy danych**: Obsługuje PostgreSQL (i jego odmiany), MS SQL Server i Big Query. Już wkrótce wsparcie dla wielu nowych rodzajów [baz danych](https://hasura.io/graphql/database/).

Przeczytaj więcej na [hasura.io](https://hasura.io) oraz w [dokumentacji](https://hasura.io/docs/).

## Spis treści
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Spis treści**

- [Silnik Hasura GraphQL](#silnik-hasura-graphql)
  - [Zalety](#zalety)
  - [Spis treści](#spis-treści)
  - [Szybki start:](#szybki-start)
    - [Wdrożenie jednym kliknięciem w Hasura Cloud](#wdrożenie-jednym-kliknięciem-w-hasura-cloud)
    - [Inne opcje wdrażania jednym kliknięciem](#inne-opcje-wdrażania-jednym-kliknięciem)
    - [Inne metody wdrażania](#inne-metody-wdrażania)
  - [Architektura](#architektura)
  - [Narzędzia po stronie klienta](#narzędzia-po-stronie-klienta)
  - [Dodaj logikę biznesową](#dodaj-logikę-biznesową)
    - [Zdalne schematy](#zdalne-schematy)
    - [Akcje](#akcje)
    - [Uruchamiaj webhooki na zdarzeniach w bazie danych](#uruchamiaj-webhooki-na-zdarzeniach-w-bazie-danych)
    - [Dane pochodne lub przekształcenia danych](#dane-pochodne-lub-przekształcenia-danych)
  - [Dema](#dema)
    - [Aplikacja na żywo](#aplikacja-na-żywo)
    - [Filmy](#filmy)
  - [Pomoc i rozwiązywanie problemów](#pomoc-i-rozwiązywanie-problemów)
  - [Bądź na bieżąco](#bądź-na-bieżąco)
  - [Rozwijanie projektu](#rozwijanie-projektu)
  - [Zasoby marki](#zasoby-marki)
  - [Licencja](#licencja)
  - [Tłumaczenia](#tłumaczenia)
 
<!-- markdown-toc end -->

## Szybki start:

### Wdrożenie jednym kliknięciem w Hasura Cloud

Najszybszym i najłatwiejszym sposobem wypróbowania Hasury jest via [Hasura Cloud](https://hasura.io/docs/latest/graphql/cloud/getting-started/index.html).

1. Kliknij poniższy przycisk, aby wdrożyć silnik GraphQL w Hasura Cloud, w tym dodatek Postgres lub korzystając z istniejącej bazy danych Postgres:

    [![Deploy to Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/signup)

1. Otwórz konsolę Hasura

   Kliknij przycisk „Uruchom konsolę”, aby otworzyć konsolę Hasura.

1. Zrób swoje pierwsze zapytanie GraphQL

   Utwórz tabelę i natychmiast uruchom pierwsze zapytanie. Obserwuj to [simple guide](https://hasura.io/docs/latest/graphql/core/getting-started/first-graphql-query.html).

### Inne opcje wdrażania jednym kliknięciem

Zapoznaj się z instrukcjami dotyczącymi następujących opcji wdrażania jednym kliknięciem:

| **Dostawca usługi** | **Link do uruchomienia jednym kliknięciem** | **Dodatkowe informacje** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [docs](https://hasura.io/docs/latest/graphql/core/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://hasura.io/docs/latest/graphql/core/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://hasura.io/docs/latest/graphql/core/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [docs](https://hasura.io/docs/latest/graphql/core/guides/deployment/render-one-click.html) |

### Inne metody wdrażania

Aby zapoznać się z wdrażaniem opartym na platformie Docker i zaawansowanymi opcjami konfiguracji, zobacz [przewodniki wdrożenia](https://hasura.io/docs/latest/graphql/core/getting-started/index.html) lub
[manifest instalacji](install-manifests).

## Architektura

Hasura GraphQL Engine obsługuje instancję bazy danych Postgres i może akceptować żądania GraphQL z aplikacji klienckich. Można go skonfigurować do pracy z istniejącym systemem uwierzytelniania i obsługiwać kontrolę dostępu za pomocą reguł na poziomie pola ze zmiennymi dynamicznymi z systemu uwierzytelniania.

Można również scalić zdalne schematy GraphQL i zapewnić ujednolicone API GraphQL.

![Hasura GraphQL Engine architecture](assets/hasura-arch.svg)

## Narzędzia po stronie klienta

Hasura współpracuje z każdym klientem GraphQL. Zobacz [awesome-graphql](https://github.com/chentsulin/awesome-graphql) dla listy klientów. Nasza [seria samouczków frontendowych](https://hasura.io/learn/#frontend-tutorial) mieć również integracje z klientami GraphQL dla różnych frameworków.

## Dodaj logikę biznesową

GraphQL Engine zapewnia łatwe do uzasadnienia, skalowalne i wydajne metody dodawania niestandardowej logiki biznesowej do zaplecza:

### Zdalne schematy

Dodaj niestandardowe programy rozpoznawania nazw w zdalnym schemacie oprócz opartego na bazie danych schematu GraphQL firmy Hasura. Idealny do zastosowań, takich jak wdrażanie interfejsu API płatności lub wysyłanie zapytań o dane, których nie ma w Twojej bazie danych - [czytaj więcej](remote-schemas.md).

### Akcje

Akcje to sposób na rozszerzenie schematu Hasury o niestandardową logikę biznesową za pomocą niestandardowych zapytań i mutacji. Do Hasury można dodać akcje, aby obsługiwać różne przypadki użycia, takie jak walidacja danych, wzbogacanie danych ze źródeł zewnętrznych i każda inna złożona logika biznesowa - [czytaj więcej](https://hasura.io/docs/latest/graphql/core/actions/index.html)

### Uruchamiaj webhooki na zdarzeniach w bazie danych

Dodaj asynchroniczną logikę biznesową wyzwalaną na podstawie zdarzeń bazy danych.
Idealny do powiadomień, potoków danych z Postgres lub asynchronicznych
przetwarzanie - [czytaj więcej](event-triggers.md).

### Dane pochodne lub przekształcenia danych

Przekształć dane w Postgresie lub uruchom na nich logikę biznesową, aby uzyskać inny zestaw danych, który można przeszukiwać za pomocą GraphQL Engine - [czytaj więcej](https://hasura.io/docs/latest/graphql/core/queries/derived-data.html).

## Dema

Sprawdź wszystkie przykładowe aplikacje w folderze [społeczności/przykładowe aplikacje](community/sample-apps).

### Aplikacja na żywo

- Aplikacja czatu grupowego zbudowana w React, zawiera wskaźnik pisania, użytkowników online i nowość powiadomienia o wiadomościach.
  - [Sprawdź](https://realtime-chat.demo.hasura.io/)
  - [Poradnik](community/sample-apps/realtime-chat)

- Aplikacja do śledzenia lokalizacji na żywo, która pokazuje działający pojazd zmieniający aktualny GPS współrzędne poruszające się po mapie.
  - [Sprawdź](https://realtime-location-tracking.demo.hasura.io/)
  - [Poradnik](community/sample-apps/realtime-location-tracking)

- Pulpit nawigacyjny w czasie rzeczywistym do agregacji danych na stale zmieniających się danych.
  - [Sprawdź](https://realtime-poll.demo.hasura.io/)
  - [Poradnik](community/sample-apps/realtime-poll)

### Filmy

* [Dodaj GraphQL do własnej instancji GitLab](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Aplikacja Todo z obsługą Auth0 i GraphQL](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GraphQL na GitLab zintegrowany z GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [Panel dla 10 milionów przejazdów z geolokalizacją (PostGRES, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)

## Pomoc i rozwiązywanie problemów

Dokumentacja i społeczność pomogą Ci rozwiązać większość problemów. Jeśli napotkałeś błąd lub potrzebujesz się z nami skontaktować, możesz skontaktować się z nami za pomocą jednego z następujących kanałów:

* Pomoc i informacje zwrotne: [Discord](https://discord.gg/hasura)
* Błędy i śledzenie błędów: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Śledź aktualizacje produktu: [@HasuraHQ](https://twitter.com/hasurahq)
* Porozmawiaj z nami na [czacie](https://hasura.io)

Zależy nam na wspieraniu otwartego i przyjaznego środowiska w społeczności. Proszę zobaczyć [Kodeks postępowania](code-of-conduct.md).

Jeśli chcesz zgłosić problem z bezpieczeństwem, proszę [przeczytać informacje z tego linka](SECURITY.md).

## Bądź na bieżąco

Co miesiąc udostępniamy nowe funkcje. Zapisz się do naszego newslettera, korzystając z poniższego linku. Newslettery wysyłamy tylko raz w miesiącu.
[https://hasura.io/newsletter/](https://hasura.io/newsletter/)

## Rozwijanie projektu

Sprawdź nasz [przewodnik dodawania własnego wkładu](CONTRIBUTING.md) po więcej informacji.

## Zasoby marki

Zasoby marki Hasura (logo, maskotka Hasura, odznaki itp.) mogą być
znaleźć w folderze [assets/brand](assets/brand). Zapraszam do korzystania z nich w swoim aplikacja/strona internetowa itp. Bylibyśmy zachwyceni, jeśli dodasz „Powered by Hasura” znaczek do Twoich aplikacji zbudowanych przy użyciu Hasury. ❤️

<div style="display: flex;">
  <img src="assets/brand/powered_by_hasura_primary_darkbg.svg" width="150px"/>
  <img src="assets/brand/powered_by_hasura_primary_lightbg.svg" width="150px"/>
</div>

```html
<!-- For light backgrounds -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_primary_darkbg.svg" />
</a>

<!-- For dark backgrounds -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_primary_lightbg.svg" />
</a>
```

## Licencja

Podstawowy silnik GraphQL jest dostępny na licencji [Apache License 2.0] (https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Cała **inna zawartość** (z wyjątkiem tych w [`server`](server), [`cli`](cli) i
[`console`](console) katalogi) są dostępne w ramach [Licencji MIT](LICENSE-community).
Obejmuje to wszystko w [`docs`](docs) i [`community`](community)
katalogi.

## Tłumaczenia

Ten plik readme jest dostępny w następujących tłumaczeniach:

- [Japanese :jp:](translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [French :fr:](translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Bosnian :bosnia_herzegovina:](translations/README.bosnian.md) (:pray: [@hajro92](https://github.com/hajro92))
- [Russian :ru:](translations/README.russian.md) (:pray: [@highflyer910](https://github.com/highflyer910))
- [Greek 🇬🇷](translations/README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))
- [Spanish 🇲🇽](/translations/README.mx_spanish.md)(:pray: [@ferdox2](https://github.com/ferdox2))
- [Indonesian :indonesia:](translations/README.indonesian.md) (:pray: [@anwari666](https://github.com/anwari666))
- [Brazilian Portuguese :brazil:](translations/README.portuguese_br.md) (:pray: [@rubensmp](https://github.com/rubensmp))
- [German 🇩🇪](translations/README.german.md) (:pray: [@FynnGrandke](https://github.com/FynnGrandke))
- [Chinese :cn:](translations/README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))
- [Turkish :tr:](translations/README.turkish.md) (:pray: [@berat](https://github.com/berat))
- [Korean :kr:](translations/README.korean.md) (:pray: [@라스크](https://github.com/laskdjlaskdj12))
- [Polish :pl:](translations/README.polish.md) (:pray: [@kry008](https://github.com/kry008))

Tłumaczenia innych plików można znaleźć [tutaj](translations).
