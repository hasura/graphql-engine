# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine, veritaban olaylarında webhook tetikleyici ve iş mantığı için uzaktan şemalar ile size **Postgres üzerinden anında, gerçek zamanlı GraphQL API**'leri sağlayan hızlı bir GraphQL sunucusudur.

Hasura, Postgres tarafından desteklenen GraphQL uygulamaları yaratmanıza veya Postgress kullananan mevcut uygulamaları aşamalı olarak GraphQL'e geçmenize yardımcı olur.

Daha fazla bilgiyi [hasura.io](https://hasura.io) ve [dökümanda](https://docs.hasura.io) bulabilirsiniz

------------------

![Hasura GraphQL Engine Demo](assets/demo.gif)

------------------

![Hasura GraphQL Engine Gerçek Zamanlı Demo](assets/realtime.gif)

-------------------

## Özellikler

* **Güçlü sorgular yapın**: Dahili filtreleme, sayfalandırma, model arama, yığın ekleme, güncelleme, mutasyonları silme
* **Gerçek zamanlı**: Abonelikleri kullanarak herhangi bir GraphQL sorgusunu canlı sorguya dönüştürme
* **Uzaktan şemaları birleştirme**: Tek bir GraphQL Engine uç noktası üzerinden iş mantığı için özel GraphQL şemalarına erişin.[**Daha fazlası**](remote-schemas.md).
* **Webhooks ve sunucusuz işlevleri tetikleme**: On Postgres insert/update/delete events ([daha fazlası](event-triggers.md))
* **Mevcut, canlı veritabanlarıyla çalışır**: Kullanıma hazır bir GraphQL API'si almak için mevcut bir Postgres veritabanına yönlendirin
* **Hasas erişim kontrolü**: Kimlik doğrulama sisteminizle birleşen dinamik erişim kontrolü (örn: auth0, firebase-auth)
* **Yüksek performans ve az yer kaplama**: ~15mb docker image; ~50MB RAM @ 1000 req/s; multi-core aware
* **Yönetici Kullanıcı Arayüzü ve Taşıma İşlemleri**: Yönetici arayüzünden ve Rails'ten ilham alan şema taşıma işlemleri
* **Postgres** ❤️: Postgres türlerini (PostGIS/geo-location vb.) destekler, görünümleri grafiklere dönüştürür, depolanmış fonksiyonları veya mutasyonlu prosedürleri tetikler

Daha fazla bilgiyi [hasura.io](https://hasura.io) ve [dökümanda](https://docs.hasura.io) bulabilirsiniz

## İçerikler
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**İçerikler**

- [Hızlı Başlangıç:](#quickstart)
    - [Tek tıklamayla Heroku'ya deploy etme](#one-click-deployment-on-heroku)
    - [Diğer deploy etme yöntemleri](#other-deployment-methods)
- [Yapı](#architecture)
- [İstemci tarafı takımlar](#client-side-tooling)
- [İş mantığı ekleme](#add-business-logic)
    - [Uzak şemalar](#remote-schemas)
    - [Veritabanı olaylarındaki webhooks tetikleme](#trigger-webhooks-on-database-events)
- [Demolar](#demos)
    - [Gerçek uygulamalar](#realtime-applications)
    - [Videolar](#videos)
- [Destek ve sorun giderme](#support--troubleshooting)
- [Katkı](#contributing)
- [Marka varlıkları](#brand-assets)
- [Lisans](#license)
- [Çeviriler](#translations)

<!-- markdown-toc end -->

## Hızlı Başlangıç:

### Tek tıklama ile Heroku'ya deploy etme

Hasura'yı denemenin en hızlı yolu Heroku'dur.

1. Ücretsiz Postgre eklentisiyle Heroku'da GraphQL Engine'i kurmak için aşağıdaki düğmeye tıklayın.

    [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Hasura Console'u açın.

   Yönetici konsolunu açmak için `https://<app-name>.herokuapp.com` (*\<app-name\>'i uygulama adınızla değiştirin.*) adresini ziyaret edin.

3. İlk GraphQL sorgunuzu yapın.

   Bir tablo oluşturun ve ilk sorgunuzu hemen çalıştırın. Bunu takip edebilirsin [simple guide](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Diğer tek tıklamayla deploy etme seçenekleri

Tek tıklamayla deploy etme seçenekleri için aşağıdaki talimatları takip edin:

| **Sağlayıcı** | **Tek tıklama linki** | **Ek bilgi** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [doküman](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [doküman](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |

### Diğer deploy etme yöntemleri

Docker tabanlı dağıtım ve gelişmiş yapılandırma seçenekleri için [deployment
guides](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html) ya da 
[install manifests](install-manifests) bakınız.

## Yapı

Hasura GraphQL Engine, bir Postgres veritabnı örneğini önler ve istemci uygulamarınızdan HraphQL isteklerini kabul edebilir. Mevcut kimlik doğrulama sisteminizle çalışacak şekilde yapılandırılabilir ve kimlik doğrumaa sisteminizden dinamik değişkenlerle alan düzeyinde kurallar kullanarak erişim denetimini işleyebilir. 

Ayrıca uzaktan GraphQL şemalarını birleştirebilir ve birleşik GraphQL API'si sağlayabilirsiniz.

![Hasura GraphQL Engine architecture](assets/hasura-arch.svg)

## İstemci tarafı takımlar

Hasura herhangi bir GraphQL istemcisi ile çalışır. [Apollo Client](https://github.com/apollographql/apollo-client) kullanmanızı öneririz. İstemci bir listesi için [awesome-graphql](https://github.com/chentsulin/awesome-graphql) adresine bakınız.

## Add business logic

GraphQL Engine provides easy-to-reason, scalable and performant methods for adding custom business logic to your backend:

### Remote schemas

Add custom resolvers in a remote schema in addition to Hasura's Postgres-based GraphQL schema. Ideal for use-cases like implementing a payment API, or querying data that is not in your database - [read more](remote-schemas.md).

### Trigger webhooks on database events

Add asynchronous business logic that is triggered based on database events.
Ideal for notifications, data-pipelines from Postgres or asynchronous
processing - [read more](event-triggers.md).

### Derived data or data transformations

Transform data in Postgres or run business logic on it to derive another dataset that can be queried using GraphQL Engine - [read more](https://docs.hasura.io/1.0/graphql/manual/queries/derived-data.html).

## Demos

Check out all the example applications in the

[community/sample-apps](community/sample-apps) directory.


### Realtime applications

- Group Chat application built with React, includes a typing indicator, online users & new
  message notifications.
  - [Try it out](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-chat)
  - [Browse APIs](https://realtime-chat.demo.hasura.app/console)

- Live location tracking app that shows a running vehicle changing current GPS
  coordinates moving on a map.
  - [Try it out](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-location-tracking)
  - [Browse APIs](https://realtime-location-tracking.demo.hasura.app/console)

- A realtime dashboard for data aggregations on continuously changing data.
  - [Try it out](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-poll)
  - [Browse APIs](https://realtime-poll.demo.hasura.app/console)

### Videos

* [Add GraphQL to a self-hosted GitLab instance](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Todo app with Auth0 and GraphQL backend](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GraphQL on GitLab integrated with GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [Dashboard for 10million rides with geo-location (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


## Support & Troubleshooting

The documentation and community will help you troubleshoot most issues. If you have encountered a bug or need to get in touch with us, you can contact us using one of the following channels:

* Support & feedback: [Discord](https://discord.gg/vBPpJkS)
* Issue & bug tracking: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Follow product updates: [@HasuraHQ](https://twitter.com/hasurahq)
* Talk to us on our [website chat](https://hasura.io)

We are committed to fostering an open and welcoming environment in the community. Please see the [Code of Conduct](code-of-conduct.md).

If you want to report a security issue, please [read this](SECURITY.md).

## Contributing

Check out our [contributing guide](CONTRIBUTING.md) for more details.

## Brand assets

Hasura brand assets (logos, the Hasura mascot, powered by badges etc.) can be
found in the [assets/brand](assets/brand) folder. Feel free to use them in your
application/website etc. We'd be thrilled if you add the "Powered by Hasura"
badge to your applications built using Hasura. ❤️

<div style="display: flex;">
  <img src="assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="assets/brand/powered_by_hasura_white.svg" width="150px"/>
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

## License

The core GraphQL Engine is available under the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

All **other contents** (except those in [`server`](server), [`cli`](cli) and
[`console`](console) directories) are available under the [MIT License](LICENSE-community).
This includes everything in the [`docs`](docs) and [`community`](community)
directories.

## Translations

This readme is available in the following translations:

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

Translations for other files can be found [here](translations).
