# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

_Hasura GraphQL Engine_ adalah sebuah _server_ GraphQL ultra-cepat yang menyediakan **API GraphQL instan dan _real-time_ diatas Postgres**, dengan [**pemicu _webhook_**](../event-triggers.md) pada _event_ basis data, dan [**skema jarak jauh**](../remote-schemas.md) untuk mengimplementasi logika bisnis.

Hasura dapat membantu Anda membangun aplikasi berbasis GraphQL dengan Postgres atau membantu aplikasi yang sudah ada untuk berpindah ke GraphQL secara bertahap.

Baca lebih lanjut di [hasura.io](https://hasura.io) dan [dokumentasinya](https://hasura.io/docs).

------------------

![Demo Hasura GraphQL Engine](../assets/demo.gif)

------------------

![Demo Hasura GraphQL Engine _Realtime_](../assets/realtime.gif)

-------------------

## Fitur

* **Buat _query_ yang andal**: Tersedia _query_ bawaan untuk menyaring, mengatur halaman, mencari pola, menyisipkan banyak sekaligus, pembaruan, penghapusan data.
* **_Realtime_**: Ubah _query_ GraphQL biasa menjadi  _live query_ dengan menggunakan _subscriptions_.
* **Gabungkan skema jarak jauh**: Akses skema _custom_ GraphQL untuk logika bisnis melalui sebuah GraphQL Engine _endpoint_. ([Baca lebih lanjut](../remote-schemas.md)).
* **Picu _webhooks_ atau fungsi _serverless_**: Pada _event_ penyisipan/perbaruan/penghapusan data di Postgres. ([Baca lebih lanjut](../event-triggers.md))
* **Dapat bekerja dengan basis data yang telah ada sebelumnya**: Arahkan pada sebuah basis data Postgres yang telah ada sebelumnya untuk mendapatkan API GraphQL yang dapat langsung digunakan. 
* **Kontrol akses yang sangat terukur**: Kontrol akses yang dinamik yang terintegrasi dengan sistem otentikasi semisal auth0 atau firebase-auth.
* **Performa tinggi & rekam jejak yang kecil**: Docker _image_ sebesar ~15MB; ~50MB RAM pada 1000 req/detik; sadar _multi-core_.
* **Antarmuka untuk admin & proses migrasi**: Antarmuka untuk admin & proses migrasi yang terinspirasi dari Rails.
* **Postgres** ❤️: Mendukung _types_ dari Postgres (PostGIS/geo-location, dll.), ubah _views_ menjadi *graphs*, memicu fungsi tersimpan (_stored functions_) atau prosedur dengan mutasi.

Baca lebih lanjut di [hasura.io](https://hasura.io) dan [dokumentasinya](https://hasura.io/docs).

## Daftar isi
<!-- markdown-toc **start** - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Daftar isi**
- [Mulai dengan cepat:](#mulai-dengan-cepat)
  - [Jalankan dengan satu klik di Heroku](#jalankan-dengan-satu-klik-di-heroku)
  - [Pilihan menjalankan aplikasi dengan satu klik lainnya](#pilihan-menjalankan-aplikasi-dengan-satu-klik-lainnya)
  - [Metode menjalankan lainnya](#metode-menjalankan-lainnya)
- [Arsitektur](#arsitektur)
- [Kakas pada sisi klien](#kakas-pada-sisi-klien)
- [Tambahkan logika bisnis](#tambahkan-logika-bisnis)
  - [Skema jarak jauh](#skema-jarak-jauh)
  - [Picu _webhooks_ pada _event_ basis data](#picu-webhooks-pada-event-basis-data)
  - [Data turunan atau transformasi data](#data-turunan-atau-transformasi-data)
- [Demo](#demo)
  - [Aplikasi _realtime_](#aplikasi-realtime)
  - [Video](#video)
- [Dukungan & Penyelesaian masalah](#dukungan--penyelesaian-masalah)
- [Berkontribusi](#berkontribusi)
- [Aset merek](#aset-merek)
- [Lisensi](#lisensi)

<!-- markdown-toc end -->

## Mulai dengan cepat:

### Jalankan dengan satu klik di Heroku

Cara paling cepat untuk mencoba Hasura adalah menggunakan Heroku.

1. Klik pada tombol dibawah ini untuk menjalankan GraphQL Engine di Heroku dengan _add-on_ Postgres secara gratis:

    [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Buka konsol Hasura

    Kunjungi `https://<nama-aplikasi>.herokuapp.com` (*ganti \<nama-aplikasi\> dengan nama aplikasi Anda*) untuk membuka konsol admin.

3. Buat _query_ GraphQL pertama Anda

    Buat sebuah tabel dan langsung jalankan _query_ pertama Anda. Ikuti [petunjuk sederhana ini](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Pilihan menjalankan aplikasi dengan satu klik lainnya

Coba lihat pilihan lainnya untuk menjalankan aplikasi dengan satu klik:

| **Penyedia infrastruktur** | **Tautan satu klik** | **Informasi tambahan** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [dokumentasi](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [dokumentasi](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |

### Metode menjalankan lainnya

Untuk menjalankan berbasis Docker dan konfigurasi lebih lanjut, coba lihat [petunjuk menjalankan](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) atau
[manifestasi instalasi](../install-manifests).

## Arsitektur

Hasura GrapQL Engine berada didepan sebuah basis data Postgres dan dapat menerima permintaan berbasis GraphQL dari aplikasi Anda. Hasura dapat dikonfigurasi untuk bekerja dengan sistem otentikasi yang telah Anda miliki dan dapat mengontrol akses menggunakan aturan pada level _field_ dengan variabel dinamis dari sistem otentikasi Anda.

Anda juga dapat menggabungkan skema GraphQL dan menyediakn sebuah API GraphQL yang terintegrasi.

![Hasura GraphQL Engine architecture](../assets/hasura-arch.svg)

## Kakas pada sisi klien

Hasura dapat tersambung dengan baik menggunakan kakas klien GraphQL apa saja. Kami merekomendasikan untuk menggunakan [Apollo Client](https://github.com/apollographql/apollo-client). Coba lihat [awesome-graphql](https://github.com/chentsulin/awesome-graphql) untuk melihat daftar klien.

## Tambahkan logika bisnis

GraphQL Engine menyediakan metode yang gampang masuk akal, terukur, dan andal dalam menambahkan logika bisnis _custom_ pada _backend_ Anda.

### Skema jarak jauh

Tambahkan _custom resolvers_ pada sebuah skema jarak jauh berdampingan dengan skema GraphQL berbasis Postgres dari Hasura. Hal ini ideal untuk kasus ketika misalnya menambahkan API pembayaran, atau meng-_query_ data yang tidak berada pada basis data Anda - [baca lebih lanjut](../remote-schemas.md).

### Picu _webhooks_ pada _event_ basis data

Tambahkan logika bisnis asinkron yang dipicu oleh _event_ basis data. Ideal untuk notifikasi, _pipeline_ data dari Postgres, atau pemrosesan secara asinkron - [baca lebih lanjut](../event-triggers.md).

### Data turunan atau transformasi data

Transformasi data di Postgres atau jalankan logika bisnis untuk menghasilkan set data turunan lainnya yang dapat di-_query_ menggunakan GraphQL Engine -  [baca lebih lanjut](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## Demo

Coba lihat seluruh contoh aplikasi pada direktori [_community/sample-apps_](../community/sample-apps).

### Aplikasi _realtime_

- Aplikasi _chat_ untuk grup yang dibuat menggunakan React. Termasuk indikator pengetikan, pengguna _online_ & notifikasi pesan baru.
  - [Coba aplikasinya](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-chat)
  - [Lihat-lihat API](https://realtime-chat.demo.hasura.app/console)

- Aplikasi pelacak yang menyiarkan lokasi secara langsung dengan memperlihatkan kendaraan berjalan dengan posisi koordinat GPS yang berubah-ubah pada peta.
  - [Coba aplikasinya](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-location-tracking)
  - [Lihat-lihat API](https://realtime-location-tracking.demo.hasura.app/console)

- Sebuah dasbor _realtime_ untuk meng-agregasi data yang berubah secara terus menerus.
  - [Coba aplikasinya](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](../community/sample-apps/realtime-poll)
  - [Lihat-lihat API](https://realtime-poll.demo.hasura.app/console)

### Video

* [Tambahkan GraphQL pada sebuah _instance_ Gitlab yang di-_hosting_ sendiri](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 menit*)
* [Aplikasi Todo dengan Auth0 dan _backend_ GraphQL](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 menit*)
* [GraphQL di GitLab yang terintegrasi dengan GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 menit*)
* [Dasbor untuk 10 juta kendaraan dengan geolokasi (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 menit*)


## Dukungan & Penyelesaian masalah

Dokumentasi & komunitas Hasura dapat membantu menyelesaikan sebagian besar masalah Anda. Jika Anda menemukan sebuah _bug_ atau memerlukan kontak langsung, Anda dapat mengontak kami melalui salah satu cara dibawah ini:

* Dukungan & _feedback_: [Discord](https://discord.gg/vBPpJkS)
* Permasalahan & pelacakan _bug_: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Ikuti perkembangan produk: [@HasuraHQ](https://twitter.com/hasurahq)
* Bicara dengan kami melalui [_website chat_](https://hasura.io)

Kami berkomitmen untuk membina lingkungan yang terbuka dan menyambut komunitas. Silakan lihat [Kode Etik](../code-of-conduct.md).

Jika Anda ingin melaporkan sebuah permasalahan keamanan, mohon [baca ini](../SECURITY.md).

## Berkontribusi

Coba lihat [petunjuk berkontribusi](../CONTRIBUTING.md) kami untuk detail lebih lanjut.

## Aset merek

Aset merek Hasura (logo, maskot, lencana _powered by_, dll.) dapat ditemukan di direktori [_assets/brand_](../assets/brand). Jangan ragu untuk menggunakannya pada aplikasi/website Anda. Kami akan sangat senang jika Anda menambahkan lencana "_Powered by Hasura_" di aplikasi yang dibangun menggunakan Hasura. ❤️

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- Untuk dasar berwarna terang -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- Untuk dasar berwarna gelap -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## Lisensi

Bagian utama dari GraphQL Engine tersedia dengan lisensi [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Seluruh **konten lainnya** (selain dari direktori [`server`](../server), [`cli`](../cli) dan
[`console`](../console)) tersedia dalam lisensi [MIT License](../LICENSE-community).
Hal ini meliputi seluruh direktori [`docs`](../docs) dan [`community`](../community).
