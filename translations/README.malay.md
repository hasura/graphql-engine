# Hasura GraphQL Engine

[![Keluaran terbaru](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![Dokumen](https://img.shields.io/badge/docs-v1.x-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine adalah pelayan(server) GraphQL yang sangat pantas yang memberi anda **API GraphQL masa nyata dan pantas berbanding Postgres**, dengan [**pencetus(trigger) webhook**](event-triggers.md) pada acara pangkalan data(database), dan [**skema jauh(remote)**](remote-schemas.md) untuk logik perniagaan.

Hasura membantu anda membina aplikasi [GraphQL](https://hasura.io/graphql/) yang disokong oleh Postgres atau secara bertahap berpindah ke GraphQL untuk aplikasi yang ada menggunakan Postgres.

Baca lebih lanjut di [hasura.io](https://hasura.io) dan juga [docs](https://hasura.io/docs/).

------------------

![Hasura GraphQL Engine Demo](assets/demo.gif)

------------------

![Demo Masa Sebenar Hasura GraphQL Engine](assets/realtime.gif)

-------------------

## ciri-ciri

* **Buat pertanyaan(queries) yang hebat**: Penapisan, penomboran, carian corak, sisipan pukal, kemas kini, hapus mutasi terbina dalam
* **Realtime**: Tukarkan sebarang pertanyaan GraphQL ke pertanyaan langsung dengan menggunakan langganan(subscriptions)
* **Gabungkan skema jauh(remote)**: Akses skema GraphQL tersuai untuk logik perniagaan melalui satu titik akhir GraphQL Engine.[**Baca lebih lanjut**](remote-schemas.md).
* **Mencetuskan webhook atau fungsi tanpa pelayan(serverless)**: Pada Postgres, masukkan / kemas kini / hapus acara [Baca lebih lanjut](event-triggers.md)
* **Bekerja dengan pangkalan data(database) langsung yang ada**: Arahkan ke pangkalan data(database) Postgres yang ada untuk mendapatkan GraphQL API siap pakai dengan serta-merta
* **Kawalan akses halus**: Kawalan akses dinamik yang berintegrasi dengan sistem autentikasi anda (eg: auth0, firebase-auth)
* **Berprestasi tinggi & jejak kaki rendah(foorprint)**: ~ Gambar pelabuhan 15MB; ~ RAM 50MB @ 1000 req / s; pelbagai teras sedar
* **UI Admin & Migrasi**: Migrasi skema yang diilhamkan oleh UI Pentadbiran & Rails
* **Postgres** ❤️: Menyokong jenis Postgres (PostGIS / geo-lokasi, dll.), Mengubah pandangan menjadi *grafik*, mencetuskan fungsi atau prosedur yang tersimpan dengan mutasi

Baca lebih lanjut di [hasura.io](https://hasura.io) dan [docs](https://hasura.io/docs/).

## Isi kandungan
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Isi kandungan**

- [Permulaan pantas:](#quickstart)
    - [Penerapan satu klik di Hasura Cloud](#one-click-deployment-on-hasura-cloud)
    - [Pilihan penggunaan satu klik lain](#other-one-click-deployment-options)
    - [Kaedah penyebaran(deploy) lain](#other-deployment-methods)
- [Senibina](#Senibina)
- [Client-side tooling](#client-side-tooling)
- [Tambahkan logik perniagaan](#add-business-logic)
    - [Remote schemas](#remote-schemas)
    - [Trigger webhooks on database events](#trigger-webhooks-on-database-events)
    - [Derived data or data transformations](#derived-data-or-data-transformations)
- [Demos](#demos)
    - [Realtime applications](#realtime-applications)
    - [Videos](#videos)
- [Support & Troubleshooting](#support--troubleshooting)
- [Sumbangan](#contributing)
- [Brand assets](#brand-assets)
- [License](#license)
- [Terjemahan](#terjemahan)

<!-- markdown-toc end -->

<a name="quickstart"></a>

## Permulaan pantas:

<a name="one-click-deployment-on-hasura-cloud"></a>

### Penerapan satu klik di Hasura Cloud

Cara terpantas dan termudah untuk mencuba Hasura adalah melalui [Hasura Cloud](https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html).

1. Klik pada butang berikut untuk menggunakan mesin(deploy) GraphQL di Hasura Cloud termasuk tambahan Postgres atau menggunakan pangkalan data(database) Postgres yang ada:

    [![Terapkan(deploy) ke Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

2. Buka konsol Hasura

   Klik pada butang "Launch console" untuk membuka Hasura console.

3. Buat pertanyaan(query) GraphQL pertama anda

   Buat jadual(table) dan jalankan pertanyaan(query) pertama anda dengan serta-merta. Ikuti [panduan ringkas ini](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html).

<a name="other-one-click-deployment-options"></a>

### Pilihan penggunaan satu klik lain

Lihat petunjuk untuk pilihan penggunaan satu klik berikut:

| **Infra provider** | **One-click link** | **Maklumat tambahan** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/render-one-click.html) |

<a name="other-deployment-methods"></a>

### Kaedah penyebaran(deploy) lain

Untuk penyebaran(deploy) berasaskan Docker dan pilihan konfigurasi lanjutan, lihat [penyebaran
panduan](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) atau
[install manifests](install-manifests).

<a name="Senibina"></a>

## Senibina

Hasura GraphQL Engine menghadap contoh pangkalan data Postgres dan dapat menerima permintaan GraphQL dari aplikasi pelanggan anda. Ini dapat dikonfigurasi untuk bekerja dengan sistem autentikasi yang ada dan dapat menangani kontrol akses menggunakan aturan tingkat lapangan dengan pemboleh ubah dinamis dari sistem autentikasi Anda.
You can also merge remote GraphQL schemas and provide a unified GraphQL API.

![Hasura GraphQL Engine architecture](assets/hasura-arch.svg)

<a name="client-side-tooling"></a>

## Client-side tooling

Hasura bekerjasama dengan mana-mana pelanggan GraphQL. Kami mengesyorkan menggunakan [Apollo Client](https://github.com/apollographql/apollo-client). Lihat [awesome-graphql](https://github.com/chentsulin/awesome-graphql) untuk senarai pelanggan.

<a name="add-business-logic"></a>

## Tambahkan logik perniagaan

GraphQL Engine menyediakan kaedah yang mudah difikirkan, dapat diskala, dan berprestasi untuk menambahkan logik perniagaan tersuai ke backend anda:

<a name="remote-schemas"></a>

### Remote schemas

Tambahkan pemecah ubahsuaian dalam skema jarak jauh selain skema GraphQL berasaskan Postgres Hasura. Sesuai untuk kes penggunaan seperti melaksanakan API pembayaran, atau membuat pertanyaan data yang tidak ada dalam pangkalan data anda - [Baca lebih lanjut](remote-schemas.md).

<a name="trigger-webhooks-on-database-events"></a>

### Trigger webhooks on database events

Tambahkan logik perniagaan tak segerak yang dicetuskan berdasarkan peristiwa pangkalan data.
Sesuai untuk pemberitahuan, saluran data dari Postgres atau tidak segerak
pemprosesan - [Baca selanjutnya](event-triggers.md).

<a name="derived-data-or-data-transformations"></a>

### Derived data or data transformations

Ubah data di Postgres atau jalankan logik perniagaan di atasnya untuk mendapatkan set data lain yang dapat ditanyakan menggunakan GraphQL Engine - [Baca selanjutnya](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

<a name="demos"></a>

## Demos

Lihat semua contoh aplikasi di [community/sample-apps](community/sample-apps) direktori.

<a name="realtime-applications"></a>

### Realtime applications

- Aplikasi Perbualan Berkumpulan yang dibina dengan React, merangkumi penunjuk menaip, pengguna dalam talian & baru
   pemberitahuan mesej.
  - [Try it out](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-chat)
  - [Browse APIs](https://realtime-chat.demo.hasura.app/console)

- Aplikasi penjejakan lokasi langsung yang menunjukkan kenderaan yang sedang berjalan mengubah GPS semasa
   koordinat bergerak pada peta.
  - [Try it out](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-location-tracking)
  - [Browse APIs](https://realtime-location-tracking.demo.hasura.app/console)

- Papan pemuka masa nyata untuk penggabungan data mengenai perubahan data yang berterusan
  - [Try it out](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-poll)
  - [Browse APIs](https://realtime-poll.demo.hasura.app/console)
  
<a name="videos"></a>

### Videos

* [Tambahkan GraphQL ke instance GitLab yang dihoskan sendiri](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Todo app with Auth0 and GraphQL backend](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GraphQL on GitLab integrated with GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [Dashboard for 10million rides with geo-location (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)

<a name="support--troubleshooting"></a>

## Support & Troubleshooting

Dokumentasi dan komuniti akan membantu anda menyelesaikan kebanyakan masalah. Sekiranya anda menghadapi bug atau perlu menghubungi kami, anda boleh menghubungi kami menggunakan salah satu saluran berikut:

* Support & feedback: [Discord](https://discord.gg/hasura)
* Issue & bug tracking: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Follow product updates: [@HasuraHQ](https://twitter.com/hasurahq)
* Bercakap dengan kami di [website chat](https://hasura.io)

Kami berkomitmen untuk memupuk persekitaran terbuka dan mesra dalam komuniti. Sila lihat [Tatakelakuan](code-of-conduct.md).

Sekiranya anda ingin melaporkan masalah keselamatan, sila [baca ini](SECURITY.md).

<a name="contributing"></a>

## Sumbangan

Lihat [panduan penyumbang](CONTRIBUTING.md) kammi untuk keterangan lebih lanjut.

<a name="brand-assets"></a>

## Brand assets

Aset jenama Hasura (logo, maskot Hasura, dikuasakan oleh lencana dll) boleh
terdapat dalam fail [assets/brand](assets/brand). Jangan ragu untuk menggunakannya di dalam
aplikasi / laman web dll. Kami akan teruja jika anda menambahkan "Powered by Hasura"
lencana ke aplikasi anda yang dibina menggunakan Hasura. ❤️

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

<a name="license"></a>

## License

Engine GraphQL teras tersedia di bawah [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

All **other contents** (except those in [`server`](server), [`cli`](cli) and
[`console`](console) directories) are available under the [MIT License](LICENSE-community).
This includes everything in the [`docs`](docs) and [`community`](community)
directories.

<a name="terjemahan"></a>

## Terjemahan

Readme ini tersedia dalam terjemahan berikut:

- [Malay 🇲🇾](translations/README.malay.md) (:pray: [@asyraf-labs](https://github.com/asyraf-labs))
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

Terjemahan untuk fail lain boleh didapati [sini](translations).
