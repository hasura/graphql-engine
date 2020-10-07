# Berkontribusi untuk Hasura grapql-engine

_Pertama_: Jika anda merasa tidak nyaman mengenai bagaimana cara untuk memulai berkontribusi, sangat dipersilahkan untuk menghubungi kami pada [kanal Discord](https://discordapp.com/invite/hasura) kami pada kanal #contrib. Anda juga bisa langsung berkontribusi dan kami akan memberikan umpan balik. Jangan khawatir - hal terburuk yang mungkin terjadi adalah anda akan diminta secara sopan untuk mengubah sesuatu. Kami sangat mengapresiasi kontribusi apapun, dan kami tidak mau adanya peraturan yang sangat ketat mengenai hal tersebut.

Walaupun demikian, untuk individual yang ingin panduan lebih mengenai cara untuk berkontribusi pada projek ini, lanjutkan membaca. Dokumen ini akan menyediakan apa yang kami cari. Dengan menyebutkan beberapa poin dibawah ini, kemungkinan bahwa kami akan merge dengan cepat atau mengecek kontribusi anda akan meningkat.

## Daftar isi

[1. Kode etik ](#code-of-conduct)

[2. Gambaran repo ](#overview)

[3. Kontributor pertama kali sangat diterima! ](#first-timers)

[4. Area untuk berkontribusi ](#areas)

[5. Cara-cara untuk berkontribusi ](#ways)

[6. Pesan commit ](#commit-messages)

[7. Translasi ](#translations)

<a name="code-of-conduct"></a>

## 1. Kode etik

Tolong ikuti [Kode etik](code-of-conduct.md) kami untuk seluruh kontribusi yang dibuat untuk Hasura.

<a name="overview"></a>

## 2. Gambaran repo

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) adalah sebuah mono-repo yang terdiri dari 3 komponen. Setiap komponennya memiliki panduan kontribusi masing-masing:

1. [Server (Haskell)](server/CONTRIBUTING.md)

2. [CLI (Go)](cli/CONTRIBUTING.md)

3. [Console (JavaScript)](console/README.md#contributing-to-hasura-console)

Seluruh tiga komponennya memiliki satu versi, yang dinotasikan dengan tag git nya atau kombinasi dari nama branch dan SHA dari git commit nya.

Untuk seluruh kontribusi, sebuah PLK (Perjanjian Lisensi Kontributor) perlu di tanda tangani [disini](https://cla-assistant.io/hasura/graphql-engine) sebelum (atau sesudah) pull request nya dibuat. Sebuah bot akan memberitahu kontributor untuk menandatangani PLK melalui komen pada pull request nya, jika dibutuhkan.

<a name="first-timers"></a>

## 3. Kontributor pertama kali sangat diterima!

Kami sangat mengapresiasi kontributor pertama kali dan kami sangat senang untuk membantumu untuk memulai. Jika ada pertanyaan, silahkan kontak kami!

Anda akan menemukan seluruh isu yang pas untuk kontributor pertama kali [disini](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

<a name="areas"></a>

## 4. Area untuk berkontribusi

Tentunya, kami mengapresiasi kontribusi untuk seluruh komponen dari Hasura. Meskipun demikian, kami telah mengidentifikasi tiga area yang sangat pas untuk kontribusi sumber terbuka (open source).

### Dokumentasi

Tujuan kami adalah untuk menjaga dokumentasi kami luas dan terperbaharui. Jika anda ingin membantu kami untuk mencapainya, kami sangat senang untuk menerima berbagai kontribusi:

- Melaporkan konten yang tidak ada / hilang

- Memperbaiki kesalahan pada dokumentasi saat ini

- Membantu kami untuk menambahkan konten pada dokumentasi

Panduan untuk berkontribusi pada dokumentasi bisa ditemukan di [docs/CONTRIBUTING.indonesia.md](docs/CONTRIBUTING.indonesia.md).

### Konten komunitas

Dari saat kami meluncurkan [laman pembelajaran](https://hasura.io/learn/), kami sangat senang untuk menerima kontribusi:

- Memperbaiki kesalahan pada tutorial pembelajaran saat ini

- Menambah tutorial baru (mohon untuk mengontak kami jika anda memiliki ide untuk menghindari duplikasi)

README untuk repositori pembelajaran bisa ditemukan pada [link berikut](https://github.com/hasura/learn-graphql).

Diluar dari konten pembelajaran, kami telah mengidentifikasi tiga cara lain untuk berkontribusi dengan konten komunitas yang teknikal:

- [Kerangka awal](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [Contoh aplikasi](https://github.com/hasura/graphql-engine/tree/master/community/sample-apps)

- [Peralatan](community/tools)

Jika anda ingin berkontribusi kepada komunitas dengan cara

- menulis blogpost teknis

- menjadi pembicara pada sebuah kegiatan

- mengorganisir sebuah workshop

cek [komunitas wiki](https://github.com/hasura/graphql-engine/wiki/Community-Wiki) kami.

Sangat dipersilahkan untuk membuat pull request jika anda memiliki sesuatu untuk ditambahkan bahkan jika tidak berkaitan dengan semua yang disebutkan diatas.

### Hasura CLI

Kami memiliki beberapa isu pada CLI yang cocok dengan kontribusi sumber terbuka (open-source). Jika anda mengetahui Go atau jika anda ingin mempelajarinya dengan praktek langsung, perhatikan beberapa [isu](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22) berikut.

README untuk repositori CLI bisa ditemukan [disini](https://github.com/hasura/graphql-engine/tree/master/cli).

<a name="ways"></a>

## 5. Cara-cara untuk berkontribusi

### Melaporkan sebuah isu

- Pastikan anda menguji dengan versi rilis terakhir. Ada kemungkinan bahwa kami sudah memperbaiki sebuah bug/error yang anda alami.

- Sertakan langkah-langkah untuk mereproduksi isu-nya, bersama dengan versi Postgres,
  versi graphql-engine dan provider yang anda gunakan (Heroku, Docker, etc.).

- Tolong sertakan seluruh log dari server, jika relevan.

### Mengerjakan/menyelesaikan sebuah isu

- Kami menggunakan [Alur kerja fork-and-branch pada git](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- Tolong pastikan bahwa ada isu terkait mengenai hal yang sedang anda kerjakan.

- Jika anda sedang mengerjakan/menyelesaikan suatu isu, harap berikan komentar untuk menghindari orang lain mengerjakan hal yang sama.

- Squash commit anda dan kaitkan dengan isu terkait dengan menggunakan `fix #<issue-no>` atau `close #<issue-no>` pasa akhir dari pesan commit nya.
  Sebagai contoh: `resolve answers to everything (fix #42)` atau `resolve answers to everything, fix #42`

- Rebase master dengan branch anda sebelum membuat sebuah pull request.

<a name="commit-messages"></a>

## 6. Pesan commit

- Baris pertama harus merupakan rangkuman dari perubahan yang dibuat, dan tidak melebihi 50 
  karakter, diikuti dengan deskripsi opsional yang lebih detail mengenai perubahannya. Lihat [link berikut](https://github.com/erlang/otp/wiki/writing-good-commit-messages)
  untuk informasi lebih mengenai cara menulis pesan commit yang lebih baik.

- Gunakan *present tense* seperti berikut: "add/fix/change", bukan "added/fixed/changed" maupun "adds/fixes/changes".

- Jangan mengkapitalkan huruf pertama pada baris rangkuman.

- Jangan tambahkan titik (.) pada akhir dari baris rangkuman.

<a name="translations"></a>

## 7. Translasi

Dokumen ini tersedia dalam bahasa berikut:

 - [Bahasa Inggris](../CONTRIBUTING.md)

(Credits: Beberapa bagian diadaptasi dari https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
