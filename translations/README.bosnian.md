# Hasura GraphQL Engine

[![Dokumenti](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine je nevjerovatno brzi GraphQL server koji vam nudi **trenutne, API-je GraphQL API-ja preko Postgres-a**, sa [**webhook pokretacima**](event-triggers.md) na desavanja u bazi podataka, i [**udaljenim semama**](remote-schemas.md) za poslovnu logiku.

Hasura vam pomaze da napravite GraphQL aplikacije podrzane Postgres-om ili postepeno predjite na GraphQL za postojece aplikacije pomocu Postgres-a.

Procitajte vise na [hasura.io](https://hasura.io) i [dokumentima](https://docs.hasura.io).

------------------

![Hasura GraphQL Engine Demo](assets/demo.gif)

------------------

![Hasura GraphQL Engine Demo u stvarnom vremenu](assets/realtime.gif)

-------------------

## Znacajke

* **Napravite snazne upite**: Ugradjeno filtriranje, prelistavanje, pretrazivanje po uzorcima, grupno ubacivanje, azuriranje, brisanje mutacija
* **U stvarnom vremenu**: Pretvorite bilo koji GraphQL upit u zivi upit koristeci pretplate
* **Spajanje udaljenih sema**: Pristupite prilagodjenim GraphQL semama za poslovnu logiku putem jedne krajnje tacke GraphQL Engine-a. [**Procitaj vise**](remote-schemas.md).
* **Okinite webhook ili funkcije bez servera**: Na Postgres sacuvaj/azuriraj/izbrisi desavanjima ([procitaj vise](event-triggers.md))
* **Radi s postojecim, aktivnim bazama podataka**: Usmjerite ga na postojecu Postgres bazu podataka da odmah dobijete spreman na koristenje GraphQL API
* **Fina kontrola pristupa**: Dinamicka kontrola pristupa koja se integrise na vas sistem pristupa (npr: auth0, firebase-auth)
* **Visoke performanse i mali tragovi**: ~15MB docker slika; ~50MB RAM @ 1000 zahtjeva/s; vise jezgra
* **Administratorsko sucelje & Migracije**: Administratorska ploca & Rails-inspirisana migracija seme
* **Postgres** ❤️: Podrzava Postgres tipove (PostGIS/geo-location, i sl.), pretvara poglede u *grafikone*, pokrece pohranjene funkcije ili procedure s mutacijama

Procitaj vise na [hasura.io](https://hasura.io) i [dokumentima](https://docs.hasura.io).

## Tabela sadrzaja
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Tabela sadrzaja**

- [Brzi pocetak:](#quickstart)
    - [Implementacija jednim klikom na Heroku](#one-click-deployment-on-heroku)
    - [Ostale metode implementacije](#other-deployment-methods)
- [Arhitektura](#architecture)
- [Klijentski alati](#client-side-tooling)
- [Dodavanje poslovne logike](#add-business-logic)
    - [Daljinske seme](#remote-schemas)
    - [Pokretanje webhook-ova na desavanjima u bazi podataka](#trigger-webhooks-on-database-events)
- [Demo](#demos)
    - [Aplikacije u stvarnom vremenu](#realtime-applications)
    - [Video](#videos)
- [Podrska & Rjesavanje problema](#support--troubleshooting)
- [Doprinos](#contributing)
- [Brendirani aseti](#brand-assets)
- [Licenca](#license)
- [Prijevodi](#translations)

<!-- markdown-toc end -->

## Brzi pocetak:

### Implementacija jednim klikom na Heroku

Najbrzi nacin da isprobate Hasura je putem Heroku-a.

1. Klikni na link ispod da postavis GraphQL Engine na Heroku uz besplatan Postgres dodatak:

    [![Postavi na Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Otvori Hasura konzolu

   Posjeti `https://<app-name>.herokuapp.com` (*zamijeni \<app-name\> sa nazivom svoje aplikacije*) da otvoris administratorsku konzolu.

3. Napravi prvi GraphQL upit

   Kreiraj tabelu i odmah isprobaj svoj prviupit. Prati ovaj [jednostavni vodic](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Ostale medote implementacije jednim klikom

Pogledaj instrukcije za ostale mogucnosti implementacije jednim klikom:

| **Infra provajder** | **Veza** | **Dodatne informacije** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |

### Ostale metode implementacije

Za Docker-baziranu implementaciju i napredne konfiguracije, pogledajte [implementacijski vodic](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html) ili
[instalirajte manifest](install-manifests).

## Arhitektura

Hasura GraphQL Engine frontira se na Postgres instancu i prihvata GraphQL zahtjeve sa vasih klijentskih aplikacija. Moze se konfigurisati da koristi vas postojeci sistem autentikacije i moze upravljati kontrolom pristupa koristeci pravila na razini polja s dinamickim varijablama vaseg sistema autentikacije.

Takodjer mozete spojiti daljinske GraphQL seme i pruziti objedinjeni GraphQL API.

![Hasura GraphQL Engine arhitektura](assets/hasura-arch.svg)

## Alati na klijentskoj strani

Hasura radi sa bilo kojim GraphQL klijentom. Mi preporucujemo [Apollo Client](https://github.com/apollographql/apollo-client). Pogledajte [awesome-graphql](https://github.com/chentsulin/awesome-graphql) listu klijenata.

## Dodavanje poslovne logike

GraphQL Engine pruza jednostavne, skalabilne i izvodljive metode za dodavanje prilagodjene poslovne logike u vas backend:

### Daljinske seme

Dodajte prilagodjene razresivace u daljinsku semu pored Hasura-ine Postgres-bazirane GraphQL seme. Idealno za slucajeve upotrebe poput implementacije API-ja za placanje, ili upita za podatke koji nisu u vasoj bazi podataka - [procitaj vise](remote-schemas.md).

### Pokrenite webhook na desavanja u bazi podataka

Dodajte asinhronu poslovnu logiku koja se pokrece na osnovu dogadjaja u bazi podataka.
Idealno za obavijesti, propust podataka iz Postgres-a ili asinhronu
obradu - [procitaj vise](event-triggers.md).

### Izvedeni podaci ili transformacije podataka

Pretvorite podatke u Postgres ili pokrenite poslovnu logiku na njemu da biste dobili drugi skup podataka koji se moze zatraziti koristeci GraphQL Engine - [procitaj vise](https://docs.hasura.io/1.0/graphql/manual/queries/derived-data.html).

## Demo

Pogledajte sve primjere dostupne u
[community/examples](community/examples) mapi.

### Stvarne aplikacije

- Aplikacija za grupno dopisivanje koja koristi React, ukljucuje pokazatelj tipkanja, online korisnike & obavijesti o novim porukama.
  - [Isprobajte odmah](https://realtime-chat.demo.hasura.app/)
  - [Tutorijal](community/sample-apps/realtime-chat)
  - [Pogledaj API](https://realtime-chat.demo.hasura.app/console)

- Aplikacija za pracenje lokacije vozila uzivo koja prikazuje GPS koordinate dok se vozilo krece po mapi..
  - [Isprobajte odmah](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorijal](community/sample-apps/realtime-location-tracking)
  - [Pogledaj API](https://realtime-location-tracking.demo.hasura.app/console)

- Nadzorna ploca u stvarnom vremenu koja objedinjuje podatke o kontinuirano mijenjanim podacima.
  - [Isprobajte odmah](https://realtime-poll.demo.hasura.app/)
  - [Tutorijal](community/sample-apps/realtime-poll)
  - [Pogledaj API](https://realtime-poll.demo.hasura.app/console)

### Video

* [Dodajte GraphQL u samostojecu GitLab instancu](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 min*)
* [Todo aplikacija sa Auth0 i GraphQL backendom](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 min*)
* [GraphQL na GitLab-u integrirana sa GitLab autentikacijom](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 min*)
* [Nadzorna ploca za 10miliona voznji sa geografskom lokacijom (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 min*)


## Podrska & Rjesavanje problema

Dokumentacija i zajednica ce vam pomoci da rijesite mnogo problema. Ako pronadjete gresku ili zelite da nas kontaktirate, mozete nas pronaci na kanalima ispod:

* Podrska & povratne informacije: [Discord](https://discord.gg/vBPpJkS)
* Problem & pracenje gresaka: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Pratite azuriranja o proizvodima: [@HasuraHQ](https://twitter.com/hasurahq)
* Pricajte s nama na [nasoj web lokaciji](https://hasura.io)

Zalazemo se za njegovanje otvorenog i dobrodoslog okruzenja u zajednici. Molimo da pogledate [kodeks ponasanja](code-of-conduct.md).

Ako zelite prijaviti sigurnosni propust, molimo [procitajte ovo](SECURITY.md).

## Doprinos

Pogledajte nas [vodic za doprinos](CONTRIBUTING.md) za vise detalja.

## Brendirani sadrzaj

Hasura bendirani sadrzaj (logo, Hasura maskota, powered by bedzevi i slicno.) mogu biti pronadjeni u [assets/brand](assets/brand) mapi. Slobodno ih mozete koristiti u vasoj aplikaciji/web stranici. Biti ce nam drago i ako dodate "Powered by Hasura" bedz na vasu aplikaciju koju ste napravili uz pomoc Hasura-e. ❤️

<div style="display: flex;">
  <img src="assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- Za svijetle pozadine -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- Za tamne pozadine -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## Licenca

GraphQL Engine je dostupan pod [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Sav **ostali sadrzaj** (osim onog u [`server`](server), [`cli`](cli) i
[`console`](console) mapama) su dostupni pod [MIT License](LICENSE-community).
To ukljucuje sve u [`docs`](docs) i [`community`](community)
mapama.

## Prijevodi

Ova stranica je dostupna u sljedecim jezicima:

- [Japanski :jp:](translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [Francuski :fr:](translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Engleski :en:](translations/README.md)

Prijevodi za ostale fajlove mogu biti pronadjeni [ovdje](translations).
