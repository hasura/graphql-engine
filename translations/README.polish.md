# Hasura GraphQL Engine

[![Latest release](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![Docs](https://img.shields.io/badge/docs-v1.x-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine jest błyskawicznie szybkim serwerem GraphQL, który daje **natychmiastowe API GraphQL w czasie rzeczywistym nad bazą danych Postgres**, z [**webhookami**](../event-triggers.md) na zdarzeniach w bazie danych oraz [**zdalnymi schematami**](../remote-schemas.md) dla logiki biznesowej.

Hasura pomoże Ci zbudować aplikacje [GraphQL](https://hasura.io/graphql/) wspierane przez Postgres lub stopniowo przenieść się do GraphQL dla istniejących aplikacji wykorzystujących Postgres.

Więcej na [hasura.io](https://hasura.io) i w [dokumentacji](https://hasura.io/docs/).

------------------

![Demo Hasura GraphQL Engine](../assets/demo.gif)

------------------

![Demo Hasura GraphQL Engine W Czasie Rzeczywistym](../assets/realtime.gif)

-------------------

## Funkcje
* **Twórz potężne zapytania** Wbudowane filtrowanie, paginacja, wyszukiwanie wzorów, masowe wstawianie, aktualizacja, usuwanie mutacji
* **Czas Rzeczywisty** Przekonwertuj dowolne zapytanie GraphQL na zapytanie live za pomocą subskrypcji
* **Połączyć zdalne schematy** Uzyskaj dostęp do własnych schematów GraphQL dla logiki biznesowej poprzez pojedynczy punkt końcowy GraphQL Engine. [**Czytaj więcej**](../remote-schemas.md).
* **Wyzwalaj webhooki lub funkcje bezserwerowe**: Na Postgres wstawianie/aktualizacja/usuwanie zdarzeń ([czytaj więcej](../event-triggers.md))
* **Pracuje z istniejącymi, żywymi bazami danych** Wskaż na istniejącą bazę danych Postgres, aby natychmiast uzyskać gotowe do użycia API GraphQL 
* **Drobiazgowa kontrola dostępu** Dynamiczna kontrola dostępu, która integruje się z twoimi systemem uwierzytelniania (np.: auth0, firebase-auth)
* **Wysokie osiągi i niski ślad** ~15MB obrazu docker; ~50MB pamięci RAM @ 1000 req/s; świadomość wielordzeniowa
* **UI Admina  & Migracje** UI Admina i zainspirowane Rails migracje schematów
* **Postgres** ❤️: Obsługuje typy Postgres (PostGIS/geo-location, itp.), zamienia widoki na *grafy*, wyzwala przechowywane funkcje lub procedury z mutacjami

Więcej na [hasura.io](https://hasura.io) i w [dokumentacji](https://hasura.io/docs/).

## Spis treści
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Spis Treści**

- [Szybki start:](#szybki-start)
    - [Wdrożenie za pomocą jednego kliknięcia na Hasura Cloud](#wdrożenie-za-pomocą-jednego-kliknięcia-na-hasura-cloud)
    - [Inne opcje wdrożenia jednym kliknięciem](#inne-opcje-wdrożenia-jednym-kliknięciem)
    - [Inne metody wdrażanie](#inne-metody-wdrażania)
- [Architektura](#architektura)
- [Narzędzia po stronie klienta](#narzędzia-po-stronie-klienta)
- [Dodaj logikę biznesową](#dodaj-logikę-biznesową)
    - [Zdalne schematy](#zdalne-schematy)
    - [Wyzwalanie webhooków o zdarzeniach w bazie danych](#wyzwalanie-webhooków-o-zdarzeniach-w-bazie-danych)
- [Demonstracje](#demonstracje)
    - [Aplikacje w czasie rzeczywistym](#aplikacje-w-czasie-rzeczywistym)
    - [Filmy](#filmy)
- [Wsparcie i rozwiązywanie problemów](#wsparcie-i-rozwiązywanie-problemów)
- [Udział w projekcie](#udział-w-projekcie)
- [Znaki Towarowe](#znaki-towarowe)
- [Licencja](#licencja)

<!-- markdown-toc end -->
## Szybki start:

### Wdrożenie za pomocą jednego kliknięcia na Hasura Cloud

Najszybszy i najłatwiejszy sposób wypróbowania Hasury to [Hasura Cloud](https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html).

1. Kliknij na poniższy przycisk, aby zainstalować silnik GraphQL na Hasura Cloud włącznie z dodatkiem Postgres lub używając istniejącej bazy danych Postgres:

    [![Wdrożenie do Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

2. Otwórz konsolę Hasura

   Kliknij na przycisk "Uruchom konsolę", aby otworzyć konsolę Hasura.

3. Wykonaj swoje pierwsze zapytanie GraphQL

   Stwórz stół i natychmiast uruchom swoje pierwsze zapytanie. Postępuj zgodnie z tym [prostym przewodnikiem](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Inne opcje wdrożenia jednym kliknięciem

Zapoznaj się z instrukcjami dotyczącymi następujących opcji wdrożenia za pomocą jednego kliknięcia:

| **Dostawca infrastruktury** | **Wdrożenie jednym kliknięciem** | **Dodatkowe inforamcje** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![Wdroż do Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [dokumentacja](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![Wdroż do DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [dokumentacja](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Wdroż do Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [dokumentacja](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![Wdroż do Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [dokumentacja](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/render-one-click.html) |

### Inne metody wdrażanie

Aby uzyskać informacje na temat wdrażanie i zaawansowanych opcji konfiguracji w oparciu o kontenery docker, patrz [przewodniki wdrażania](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) lub
[manifesty instalacji](../install-manifests).

## Architektura

Hasura GraphQL Engine działa przed bazą danych Postgres i może akceptować żądania GraphQL z twoich aplikacji klienckich. Może być skonfigurowany do pracy z istniejącym systemem uwierzytelniania i może obsługiwać kontrolę dostępu za pomocą reguł na poziomie pola z dynamicznymi zmiennymi z systemu uwierzytelniania.

Możesz również łączyć zdalne schematy GraphQL i dostarczać ujednolicone API GraphQL.

![Architektura Hasura GraphQL Engine](../assets/hasura-arch.svg)

## Narzędzia po stronie klienta

Hasura współpracuje z dowolnym klientem GraphQL. Zalecamy korzystanie z [Apollo Client](https://github.com/apollographql/apollo-client). Zobacz listę klientów [awesome-graphql](https://github.com/chentsulin/awesome-graphql).

## Dodaj logikę biznesową

GraphQL Engine zapewnia łatwe do uzasadnienia, skalowalne i wydajne metody dodawania własnej logiki biznesowej do Twojego backendu:

### Zdalne schematy

Dodaj własne resolwery w zdalnym schemacie jako dodatek do schematu Hasura Postgres bazującego na GraphQL. Idealny dla przypadków użytkowych, takich jak implementacja API płatności, lub zapytań o dane, których nie ma w Twojej bazie danych - [czytaj więcej](../remote-schemas.md).

### Wyzwalanie webhooków o zdarzeniach w bazie danych

Dodaj asynchroniczną logikę biznesową, która jest wyzwalana na podstawie zdarzeń w bazie danych.
Idealny do powiadomień, transmisji danych z Postgres lub przetwarzaniu asynchronicznym - [czytaj dalej](../event-triggers.md).

### Pochodne dane lub transformacje danych

Przekształć dane w Postgres lub uruchom na nich logikę biznesową, aby uzyskać inny zbiór danych, który może być wyszukany za pomocą GraphQL Engine - [czytaj więcej](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## Demonstracje

Sprawdź wszystkie przykładowe aplikacje w katalogu [community/sample-apps](../community/sample-apps).

### Aplikacje w czasie rzeczywistym

- Czat grupowy zbudowany w Reactcie, zawiera wskaźnik pisania, listę użytkowników online i powiadomienia o nowych  wiadomościach.
  - [Wypróbuj to](https://realtime-chat.demo.hasura.app/)
  - [Samouczek](../community/sample-apps/realtime-chat)
  - [Przeglądaj API](https://realtime-chat.demo.hasura.app/console)

- Aplikacja do śledzenia na żywo lokalizacji pojazdu zmieniającego koordynaty GPS na mapie.
  - [Wypróbuj to](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-location-tracking)
  - [Przeglądaj API](https://realtime-location-tracking.demo.hasura.app/console)

- Aplikacja w czasie rzeczywistym do agregacji stale zmieniających się danych.
  - [Wypróbuj to](https://realtime-poll.demo.hasura.app/)
  - [Samouczek](../community/sample-apps/realtime-poll)
  - [Browse APIs](https://realtime-poll.demo.hasura.app/console)

### Filmy

* [Add GraphQL to a self-hosted GitLab instance](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Todo app with Auth0 and GraphQL backend](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GraphQL on GitLab integrated with GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [Dashboard for 10million rides with geo-location (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


## Wsparcie i rozwiązywanie problemów

Dokumentacja i społeczność pomoże Ci rozwiązać większość problemów. Jeśli napotkałeś błąd lub potrzebujesz skontaktować się z nami, możesz skontaktować się z nami za pomocą jednego z poniższych kanałów:

* Pomoc techniczna i opinie: [Discord](https://discord.gg/hasura)
* Śledzenia problemów i błędów: [Github Issues](https://github.com/hasura/graphql-engine/issues)
* Śledź aktualizacje produktu: [@HasuraHQ](https://twitter.com/hasurahq)
* Porozmawiaj z nami na naszym [czacie internetowym](https://hasura.io)

Jesteśmy zaangażowani we wspieranie otwartego i przyjaznego środowiska w społeczności. Proszę zapoznać się z [Kodeksem Postępowania](code-of-conduct.polish.md).

Jeśli chcesz zgłosić problem związany z bezpieczeństwem, prosimy o [przeczytanie tego](SECURITY.polish.md).

## Udział w projekcie

Więcej szczegółów znajdziesz w naszym [przewodniku](CONTRIBUTING.polish.md).

## Znaki Towarowe

Znaki towarowe Hasura (logo, maskotka Hasura, odznaki itp.) mogą być
znalezione się w folderze [assets/brand](../assets/brand). Zachęcamy do korzystania z nich w swojej aplikacji/stronie internetowej itp. Będziemy zachwyceni, jeśli dodasz  odznake "Powered by Hasura" do Twoich aplikacji zbudowanych przy użyciu Hasury. ❤️

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- Dla jansego tła -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- Dla ciemnego tła -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## Licencja

GraphQL Engine Core dostępny jest na licencji [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Wszystkie **inne treści** (z wyjątkiem tych w folderach [`server`](../server), [`cli`](../cli) oraz
[`console`](../console)) są dostępne w ramach [Licencji MIT](../LICENSE-community).
Obejmuje to wszystko w katalogach [`docs`](docs) i [`community`](community).
