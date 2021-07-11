# Menyumbang kepada Hasura graphql-engine

_Pertama_: jika anda tidak tahu tentang cara mula memberi sumbangan, sila tanya kami di [saluran Discord](https://discordapp.com/invite/hasura) kami di saluran #contrib. Anda juga boleh meneruskan sumbangan anda dan kami akan memberi maklum balas kepada anda. Jangan risau - yang terburuk boleh berlaku ialah anda akan diminta untuk mengubah sesuatu dengan sopan. Kami menghargai apa-apa sumbangan, dan kami tidak mahu tembok peraturan menghalangnya.

Walau bagaimanapun, bagi individu yang mahukan sedikit panduan mengenai cara terbaik untuk menyumbang kepada projek ini, baca terus. Dokumen ini akan merangkumi apa yang kami cari. Dengan menangani perkara di bawah, kemungkinan kita
dapat menggabungkan dengan cepat atau menangani sumbangan anda akan meningkat.

## Isi kandungan

[1. Code of conduct ](#code-of-conduct)

[2. Gambaran keseluruhan Repo ](#overview)

[3. Penyumbang kali pertama dialu-alukan! ](#first-timers)

[4. Kawasan untuk menyumbang ](#areas)

[5. Cara menyumbang ](#ways)

[6. Komit mesej ](#commit-messages)

[7. Terjemahan ](#translations)

<a name="code-of-conduct"></a>

## 1. Code of conduct

Sila ikuti [Tatakelakuan](code-of-conduct.md) kami dalam konteks sebarang sumbangan yang diberikan kepada Hasura.

<a name="overview"></a>

## 2. Gambaran keseluruhan Repo

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) adalah mono-repo
terdiri daripada 3 komponen. Masing-masing mempunyai panduan penyumbang sendiri:

1. [Server (Haskell)](server/CONTRIBUTING.md)

2. [CLI (Go)](cli/CONTRIBUTING.md)

3. [Console (JavaScript)](console/README.md#contributing-to-hasura-console)

Ketiga-tiga komponen mempunyai versi tunggal, dilambangkan dengan tag git atau gabungan nama cawangan dan git commit SHA.
Untuk semua sumbangan, CLA (Perjanjian Lesen Penyumbang) perlu ditandatangani [di sini](https://cla-assistant.io/hasura/graphql-engine) sebelum (atau selepas) permintaan tarik(pull-request) telah dihantar. Bot akan meminta penyumbang menandatangani CLA melalui komen permintaan tarik(pull-request), jika perlu.

<a name="first-timers"></a>

## 3. Penyumbang kali pertama dialu-alukan!

Kami menghargai penyumbang kali pertama dan kami dengan senang hati membantu anda memulakannya. Sekiranya ada pertanyaan, hubungi kami!
Anda dapati semua isu sesuai untuk penyumbang pertama [di sini](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

<a name="areas"></a>

## 4. Kawasan untuk menyumbang

Sudah tentu, kami menghargai sumbangan kepada semua komponen Hasura. Walau bagaimanapun, kami telah mengenal pasti tiga bidang yang sangat sesuai untuk sumbangan sumber terbuka.

### Dokumen(Docs)

Matlamat kami adalah untuk memastikan dokumen kami lengkap dan dikemas kini. Sekiranya anda ingin membantu kami dalam melakukannya, kami berterima kasih atas apa jua sumbangan:

- Laporkan kandungan yang hilang

- Betulkan ralat dalam dokumen yang ada

- Bantu kami dalam menambah dokumen

Panduan penyumbang dokumen boleh didapati di [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md).

### Kandungan komuniti

Sejak kami melancarkan [halaman pembelajaran](https://hasura.io/learn/) kami, kami gembira dengan sumbangan:

- Perbaiki kesilapan dalam tutorial pembelajaran yang ada

- Tambahkan tutorial baru (hubungi kami jika anda mempunyai idea untuk mengelakkan pendua kata)

README repositori pembelajaran boleh didapati [di sini](https://github.com/hasura/learn-graphql).

Selain daripada kandungan pembelajaran, kami telah mengenal pasti tiga cara lain untuk menyumbang kepada kandungan komuniti teknikal:

- [Boilerplates](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [Contoh aplikasi](https://github.com/hasura/graphql-engine/tree/master/community/sample-apps)

- [Alat(tools)](community/tools)

Sekiranya anda ingin menyumbang kepada masyarakat dengan

- menulis catatan blog teknikal

- bercakap pada satu majlis

- menganjurkan bengkel

lihat [wiki komuniti](https://github.com/hasura/graphql-engine/wiki/Community-Wiki) kami.

Jangan ragu untuk mengemukakan permintaan tarik(pull-request) sekiranya anda mempunyai sesuatu untuk ditambahkan walaupun tidak berkaitan dengan perkara yang disebutkan di atas.

### Hasura CLI

Kami mempunyai beberapa masalah mengenai CLI yang sesuai untuk sumbangan sumber terbuka. Sekiranya anda tahu Go atau jika anda ingin mempelajarinya dengan melakukannya, lihat [isu](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22) berikut.

README repositori CLI boleh didapati [di sini](https://github.com/hasura/graphql-engine/tree/master/cli).

<a name="ways"></a>

## 5. Cara menyumbang

### Melaporkan Masalah

- Pastikan anda menguji versi terbaru yang dikeluarkan. Ada kemungkinan kita mungkin telah memperbaiki bug yang anda alami.

- Berikan langkah untuk menghasilkan semula masalah, termasuk versi Postgres,
   versi graphql-engine dan penyedia(provider) yang anda jalankan (Heroku, Docker, dll.).

- Sila sertakan log pelayan(server), jika berkaitan.

### Mengusahakan isu

- Kami menggunakan [alur kerja git garpu(fork)-dan-cabang(cabang)](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- Pastikan ada masalah yang berkaitan dengan pekerjaan yang anda lakukan.

- Sekiranya anda menangani masalah, sila komen bahawa anda melakukannya untuk mengelakkan pendua karya orang lain juga.

- Tekan komit anda dan lihat masalahnya dengan menggunakan `fix #<issue-no>` atau `close #<issue-no>` pada pesan komit, di akhir.
   Contohnya: `selesaikan jawapan untuk semua perkara (perbaiki #42)` atau `selesaikan jawapan untuk semuanya, betulkan #42`

- Rebase master dengan cawangan(branch) anda sebelum menghantar permintaan tarik.

<a name="commit-messages"></a>

## 6. Komit mesej

- Baris pertama mestilah ringkasan perubahan, tidak melebihi 50
   watak, diikuti oleh badan pilihan yang mempunyai lebih banyak perincian mengenai
   perubahan. Rujuk [pautan ini](https://github.com/erlang/otp/wiki/writing-good-commit-messages)
  untuk maklumat lebih lanjut mengenai menulis mesej komited yang baik.

- Gunakan ketegangan sekarang yang mustahak: "add/fix/change", bukan "added/fixed/changed" atau "adds/fixes/changes".

- Jangan menggunakan huruf besar pada baris ringkasan.

- Jangan tambah titik (.) Di akhir baris ringkasan.

<a name="translations"></a>

## 7. Terjemahan

Dokumen ini terdapat dalam terjemahan berikut:

- [French 🇫🇷](translations/CONTRIBUTING.french.md)

(Credits: Beberapa bahagian disesuaikan dari https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
