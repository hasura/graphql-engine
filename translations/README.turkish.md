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

![Hasura GraphQL Engine Demo](../assets/demo.gif)

------------------

![Hasura GraphQL Engine Gerçek Zamanlı Demo](../assets/realtime.gif)

-------------------

## Özellikler

* **Güçlü sorgular yapın**: Dahili filtreleme, sayfalandırma, model arama, yığın ekleme, güncelleme, mutasyonları silme
* **Gerçek zamanlı**: Abonelikleri kullanarak herhangi bir GraphQL sorgusunu canlı sorguya dönüştürme
* **Uzaktan şemaları birleştirme**: Tek bir GraphQL Engine uç noktası üzerinden iş mantığı için özel GraphQL şemalarına erişin.[**Daha fazla**](../remote-schemas.md).
* **Webhooks ve sunucusuz işlevleri tetikleme**: On Postgres insert/update/delete events ([daha fazla](../event-triggers.md))
* **Mevcut, canlı veritabanlarıyla çalışır**: Kullanıma hazır bir GraphQL API'si almak için mevcut bir Postgres veritabanına yönlendirin
* **Hasas erişim kontrolü**: Kimlik doğrulama sisteminizle birleşen dinamik erişim kontrolü (örn: auth0, firebase-auth)
* **Yüksek performans ve az yer kaplama**: ~15mb docker image; ~50MB RAM @ 1000 req/s; multi-core aware
* **Yönetici Kullanıcı Arayüzü ve Taşıma İşlemleri**: Yönetici arayüzünden ve Rails'ten ilham alan şema taşıma işlemleri
* **Postgres** ❤️: Postgres türlerini (PostGIS/geo-location vb.) destekler, görünümleri grafiklere dönüştürür, depolanmış fonksiyonları veya mutasyonlu prosedürleri tetikler

Daha fazla bilgiyi [hasura.io](https://hasura.io) ve [dökümanda](https://docs.hasura.io) bulabilirsiniz

## İçerikler
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**İçerikler**

- [Hızlı Başlangıç:](#hızlı-başlangıç)
    - [Tek tıklamayla Heroku'ya deploy etme](#tek-tiklamayla-Heroku-ya-deploy-etme)
    - [Diğer deploy etme yöntemleri](#diger-deploy-etme-yontemleri)
- [Yapı](#yap)
- [İstemci tarafı takımlar](#istemci-tarafi-takimlari)
- [İş mantığı ekleme](#is-mantigi-ekleme)
    - [Uzak şemalar](#uzak-semalar)
    - [Veritabanı olaylarındaki webhooks tetikleme](#veritabani-olaylarindaki-webhooks-tetikleme)
- [Demolar](#demolar)
    - [Gerçek zamanlı uygulamalar](#gercek-zamanli-uygulamalar)
    - [Videolar](#videolar)
- [Destek ve sorun giderme](#destek-ve-sorun-giderme)
- [Katkıda bulunmak](#katkida-bulunmak)
- [Marka varlıkları](#marka-varliklari)
- [Lisans](#lisans)
- [Çeviriler](#ceviriler)

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
[install manifests](../install-manifests) bakınız.

## Yapı

Hasura GraphQL Engine, bir Postgres veritabnı örneğini önler ve istemci uygulamarınızdan HraphQL isteklerini kabul edebilir. Mevcut kimlik doğrulama sisteminizle çalışacak şekilde yapılandırılabilir ve kimlik doğrumaa sisteminizden dinamik değişkenlerle alan düzeyinde kurallar kullanarak erişim denetimini işleyebilir. 

Ayrıca uzaktan GraphQL şemalarını birleştirebilir ve birleşik GraphQL API'si sağlayabilirsiniz.

![Hasura GraphQL Engine architecture](../assets/hasura-arch.svg)

## İstemci tarafı takımlar

Hasura herhangi bir GraphQL istemcisi ile çalışır. [Apollo Client](https://github.com/apollographql/apollo-client) kullanmanızı öneririz. İstemci bir listesi için [awesome-graphql](https://github.com/chentsulin/awesome-graphql) adresine bakınız.

## İş mantığı ekleme

GraphQL Engine, backend'inize özel iş mantığı eklemek için kolay anlaşılır, ölçeklenebilir ve performans yöntemleri sunar: 

### Uzak Şemalar

Hasura'nın Postgres tabanlı GraphQL şemasına ek olarak uzak bir şemada özel çözümleyiciler ekleyin. Bir ödeme API'sini uygulamak veya veritabanımızda bulunan verileri sorgulamak gibi kullanım durumları için idealdir. - [daha fazla](../remote-schemas.md).

### Veritabanı olaylarındaki webhooks tetikleme

Veritabanı olaylarına göre tetiklenen eşzamansız iş mantığını ekleyin.
Bildirimler, Postgres veri  hatları veya asenkron işleme için idealdir - [daha fazla](../event-triggers.md).

### Türetilmiş veri veya veri dönüşümleri

Postgres'te verileri dönüştürün veya GraphQL Engine kullanılarak ssorgulanabilecek başka bir veri kümesi türetmek için iş mantığı çalıştırın. - [daha fazla](https://docs.hasura.io/1.0/graphql/manual/queries/derived-data.html).

## Demolar

Tüm örnek uygulamaları kontrol edin.

[community/sample-apps](../community/sample-apps) dizini.


### Gerçek zamanlı uygulamalar

- React ile oluşturulan grup shohbet uygulaması, bir yazma belirteci içerir, çevrimiçi kullanıcılar ve yeni mesaj bildirimleri.
  - [Deneyin](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-chat)
  - [APİ'lere göz atın](https://realtime-chat.demo.hasura.app/console)

- Bir harita üzerinde hareket eden mevcut GPS koordinatlarını değiştiren, çalışan bir aracı gösteren canlı konum izleme uygulaması.
  - [Deneyin](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-location-tracking)
  - [APİ'lere göz ayın](https://realtime-location-tracking.demo.hasura.app/console)

- Sürekli değişen veriler üzerinde veri toplama için gerçek zamanlı bir kontrol paneli.
  - [Deneyin](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-poll)
  - [APİ'lere göz atın](https://realtime-poll.demo.hasura.app/console)

### Videolar

* [Kendi kendine barındırılan bir GitLab örneğine GraphQL ekleme](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Auth0 ve GraphQL backend'iyle todo uygulaması](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GitLab auth ile entegre edilmiş GitLab üzerinde GraphQL](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [Coğrafi konumlu 10 milyon sürüş için gösterge tablosu (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


## Destek ve sorun giderme
Belgeler ve topluluğun çoğu sorunu gidermenize yardımcı olacaktır. Bir hatayla karşılaştıysanız veya bizimle iletişime geçmeniz gerekiyorsa aşağıdaki kanallardan birini kullanarak bizimle iletişime geçebilirsiniz: 

* Destek ve geri bildirim: [Discord](https://discord.gg/vBPpJkS)
* Sorun ve hata bildirme: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Ürün güncellemelerini takip edin: [@HasuraHQ](https://twitter.com/hasurahq)
* Bizimle konuşun: [website chat](https://hasura.io)

Tolumda açık ve hoş bir ortam yaratmaya kararlıyız. Lütfen [Code of Conduct](../code-of-conduct.md) adresine bakınız

Bir güvenlik sorununu bildirmek istiyorsanız lütfen [bunu okuyun](../SECURITY.md).

## Katkıda bulunmak

Daha fazla detay için [katkıda bulunma kuralını](../CONTRIBUTING.md) kontrol edin.

## Marka varlıkları

Hasura marka varlıkları (logolar, Hasura maskotu, rozetler vb.) [assets/brand](../assets/brand) klasöründedir. Bunları Uygulamanızda/websitenizde kullanmaktan çekinmeyin. Hasura kullanılarak oluşturduğunuz uygulamalarınıza "Powered by Hasura" işareti eklerseniz çok seviniriz. ❤️

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- Beyaz arkaplan için -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- Siyah arkaplan için -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## Lisans

Çekirdek GraphQL Engine [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0) altında bulunur.

**Diğer türm içerikler** ([`sunucu`](../server), [`cli`](../cli) ve
[`konsol`](../console) dizinleri hariç) [MIT License](../LICENSE-community) altındadır.
Bu, [`dokuman`](../docs) ve [`topluluk`](../community) üzerindeki her şeyi içerir.
directories.

## Çeviriler

Bu Readme sayfası aşağıdaki çevirilerde mevcuttur:

- [Japonca :jp:](translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [Fransızca :fr:](translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Boşnakça :bosnia_herzegovina:](translations/README.bosnian.md) (:pray: [@hajro92](https://github.com/hajro92))
- [Rusca :ru:](translations/README.russian.md) (:pray: [@highflyer910](https://github.com/highflyer910))
- [Yunanca 🇬🇷](translations/README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))
- [İspanyolca 🇲🇽](/translations/README.mx_spanish.md)(:pray: [@ferdox2](https://github.com/ferdox2))
- [Endonezyaca :indonesia:](translations/README.indonesian.md) (:pray: [@anwari666](https://github.com/anwari666))
- [Brezilya Portekizcesi :brazil:](translations/README.portuguese_br.md) (:pray: [@rubensmp](https://github.com/rubensmp))
- [Almanca 🇩🇪](translations/README.german.md) (:pray: [@FynnGrandke](https://github.com/FynnGrandke))
- [Çince :cn:](translations/README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))

Diğer dosyalar için çeviriler [burada](../translations) bulunabilir.
