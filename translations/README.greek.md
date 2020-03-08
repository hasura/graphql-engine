# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>


Το Hasura σε βοηθάει να δημιουργήσεις εφαρμογές GraphQL υποστηριζόμενες από το Postgres ή να μετακινήσεις διαδοχικά ήδη υπάρχουσες εφαρμογές στο GraphQL χρησιμοποιώντας το Postgres.

Διάβαστε περισσότερα στο [hasura.io](https://hasura.io) και στο [docs](https://hasura.io/docs).

------------------

![Hasura GraphQL Engine Demo](https://github.com/hasura/graphql-engine/blob/stable/assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](https://github.com/hasura/graphql-engine/blob/stable/assets/realtime.gif)

-------------------

## Χαρακτηριστικά

* **Εκτελέστε σύνθετα ερωτήματα**: Ενσωματωμένο φιλτράρισμα, σελίδoποίηση, αναζήτηση προτύπου, μαζικές προσθηήκες, ενημέρωσεις και διαγραφές.
* **Σε πραγματικό χρόνο**: Μετατρέψτε οποιοδήποτε ερώτημα GraphQL σε ερώτημα πραγματικού χρόνου χρησιμοποιώντας subscriptions
* **Συγχώνευση απομακρυσμένων σχημάτων**: Αποκτήστε πρόσβαση στα δικά σας GraphQL σχήματα για την επιχειρησιακή σας λογική μέσω ενός σημείου πρόσβασης GraphQL. [**Διαβάστε Περισσότερα**](https://github.com/hasura/graphql-engine/blob/stable/remote-schemas.md).
* **Άνοιγμα webhooks ή λειτουργίες χωρίς διακομιστή**: Στο Postgres προσθέστε/ενημερώστε/διαγράψτε γεγονότα ([Διαβάστε Περισσότερα](https://github.com/hasura/graphql-engine/blob/stable/event-triggers.md))
* **Λειτουργεί με υπάρχουσες βάσεις δεδομένων**: Υποδείξτε στη GraphQL Engine μια υπάρχουσα βάση δεδομένων Postgres για να αποκτήσετε άμεσα ένα ready-to-use GraphQL API.
* **Λεπτομερής έλεγχος πρόσβασης**: Δυναμικός έλεγχος της κίνησης του συστήματος με το σύστημα επαλήθευσης (π.χ.: auth0, firebase-auth)
* **Υψηλή απόδοση και χαμηλό αποτυπώμα**: Image Docker σε ~15MB, ~50MB RAM @ 1000 req/s; λαμβάνοντας υπόψη τον πολυπύρηνο
* **Admin UI & Migrations**: Admin UI & Rails-inspired schema migrations
* **Postgres** ❤️: Υποστηρίζει τύπους Postgres (PostGIS/geo-location, κτλ.), μετατρέπει τις προβολές σε *γραφήματα* , ενεργοποιηεί αποθηκευμένες λειτουργίες ή διαδικασίες

Διαβάστε περισσότερα στο [hasura.io](https://hasura.io) και στο [docs](https://hasura.io/docs).

## Πίνακας περιεχομένων
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Πίνακας περιεχομένων**

- [Γρήγορη εκκίνηση:](#γρήγορη-εκκίνηση)
    - [Ανάπτυξη του Heroku με ένα κλίκ](#ανάπτυξη-του-heroku-με-ένα-κλίκ)
    - [Άλλες μέθοδοι ανάπτυξης](#άλλες-μέθοδοι-ανάπτυξης)
- [Αρχιτεκτονική](#αρχιτεκτονική)
- [Εργαλεία από την πλευρά του πελάτη](#εργαλεία-από-την-πλευρά-του-πελάτη)
- [Προσθήκη επιχειρηματικής λογικής](#προσθήκη-επιχειρηματικής-λογικής)
    - [Απομακρυσμένα σχήματα](#απομακρυσμένα-σχήματα)
    - [Ενεργοποίηση webhooks σε γεγονότα της βάσης δεδομένων](#ενεργοποίηση-webhooks-σε-γεγονότα-της-βάσης-δεδομένων)
- [Demos](#demos)
    - [Εφαρμογές σε πραγματικό χρόνο](#εφαρμογές-σε-πραγματικό-χρόνο)
    - [Videos](#videos)
- [Υποστήριξη & Αντιμετώπιση προβλημάτων](#υποστήριξη--αντιμετώπιση-προβλημάτων)
- [Συνεισφορά](#συνεισφορά)
- [Στοιχεία επωνυμίας](#στοιχεία-επωνυμίας)
- [Άδεια](#άδεια)

<!-- markdown-toc end -->

## Γρήγορη εκκίνηση:

### Ανάπτυξη του Heroku με ένα κλίκ

Ο γρηγορότερος τρόπος να δοκιμάσετε το Hasura είναι μέσω του Heroku

1. Πατήστε το παρακάτω κουμπί για να ανοίξετε την GraphQL Engine στο Heroku με το δωρεάν πρόσθετο Postgres:

   [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Ανοίξτε την κονσόλα του Hasura

   Επισκεφθείτε το: `https://<app-name>.herokuapp.com` (*αντικαταστήστε το \<app-name\> με το όνομα της εφαρμογής σας*) για να ανοίξετε την κεντρική κονσόλα.

3. Κάντε το πρώτο σας ερώτημα GraphQL

   Δημιουργήστε έναν πίνακα και κάντε το πρώτο σας ερώτημα. Ακολουθήστε [αυτόν](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html) τον απλό οδηγό.
   
### Άλλες μέθοδοι ανάπτυξης σε ένα κλίκ

Δείτε τις οδηγίες για τις παρακάτω σε ένα κλίκ επιλογές ανάπτυξης:

| **Infra provider** | **One-click link** | **Additional information** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |

### Άλλες μέθοδοι ανάπτυξης

Για μεθόδους ανάπτυξης με βάση το Docker και για προχωρημένες επιλογές διαμόρφωσης, ανατρέξτε στους [οδηγούς ανάπτυξης](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) ή στα
[μηνύματα εγκατάστασης](https://github.com/hasura/graphql-engine/tree/master/install-manifests).

## Αρχιτεκτονική

Το Hasura GraphQL Engine βρίσκεται μπροστά από μια βάση δεδομένων Postgres και μπορεί να δεχτεί ερωτήματα GraphQL από τις εφαρμογές πελάτη σας. Μπορεί να ρυθμιστεί ώστε να λειτουργεί με το υπάρχον σύστημα ελέγχου ταυτότητας και μπορεί να χειριστεί τον έλεγχο πρόσβασης σε επίπεδο τομέα μέσω κανόνων, με δυναμικές μεταβλητές που προέρχονται από το σύστημα ελέγχου ταυτότητας.

Μπορείτε επίσης να συγχωνεύσετε απομακρυσμένα σχήματα GraphQL και να δώσετε ένα ενοποιημένο API GraphQL.

![Hasura GraphQL Engine architecture](https://github.com/hasura/graphql-engine/blob/stable/assets/hasura-arch.svg)

## Εργαλεία από την πλευρά του πελάτη
Το Hasura συνεργάζεται με οποιοδήποτε πελάτη GraphQL. Συνιστούμε τη χρήση του [Apollo Client](https://github.com/apollographql/apollo-client). Ελέγξτε το [awesome-graphql](https://github.com/chentsulin/awesome-graphql) για μια λίστα πελατών.

## Προσθήκη επιχειρηματικής λογικής

Το GraphQL Engine παρέχει αρκετούς σαφείς, κλιμακωτούς και ισχυρούς τρόπους για να προσθέσετε τη δική σας επιχειρησιακή λογική στο backend σας:

### Απομακρυσμένα σχήματα

Προσθέστε τους δικούς σας αναλυτές σε ένα απομακρυσμένο σχήμα εκτός από το σχήματος Postgres του Hasura. Ιδανικό για περιπτώσεις χρήσης όπως η εφαρμογή ενός API πληρωμής ή η αναζήτηση δεδομένων που δεν βρέθηκαν στη βάση δεδομένων σας - [διαβάστε περισσότερα](https://github.com/hasura/graphql-engine/blob/stable/remote-schemas.md).

### Ενεργοποίηση webhooks σε γεγονότα της βάσης δεδομένων

Προσθέστε ασύγχρονη επιχειρησιακή λογική που ενεργοποιείται από συμβάντα βάσης δεδομένων. Ιδανικό για ειδοποιήσεις, αγωγούς δεδομένων Postgres ή ασύγχρονη επεξεργασία - [διαβάστε περισσότερα](https://github.com/hasura/graphql-engine/blob/stable/event-triggers.md).

### Παραγόμενα δεδομένα ή μετασχηματισμοί δεδομένων

Μετατρέψτε τα δεδομένα στο Postgres ή τρέξτε επιχειρηματική λογική με άλλα δεδομένα που μπορούν να αναζητηθούν χρησιμοποιώντας το GraphQL Engine - [διαβάστε περισσότερα](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## Demos

Δείτε όλα τα παραδείγματα εφαρμογών στον κατάλογο
[παραδειγμάτων](https://github.com/hasura/graphql-engine/tree/stable/community/sample-apps).

### Εφαρμογές σε πραγματικό χρόνο

- Μια ομαδική εφαρμογή ανταλλαγής μηνυμάτων που αναπτύχθηκε με το React, συμπεριλαμβανομένων χαρακτηριστικών όπως δείκτη πληκτρολόγησης, ένδειξη ενεργών χρηστών και ειδοποιήσεις νέων μηνυμάτων.
  - [Δοκιμάστε το](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](https://github.com/hasura/graphql-engine/tree/stable/community/sample-apps/realtime-chat)
  - [Εξερευνήστε APIs](https://realtime-chat.demo.hasura.app/console)

- Εφαρμογή εντοπισμού θέσης σε πραγματικό χρόνο που δείχνει όχημα του οποίου οι συντεταγμένες GPS μετακινούνται συνεχώς σε χάρτη.
  - [Δοκιμάστε το](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](https://github.com/hasura/graphql-engine/tree/stable/community/sample-apps/realtime-location-tracking)
  - [Εξερευνήστε APIs](https://realtime-location-tracking.demo.hasura.app/console)

- Πίνακας ελέγχου σε πραγματικό χρόνο για συνεχώς μεταβαλλόμενη συνάθροιση δεδομένων.
  - [Δοκιμάστε το](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](https://github.com/hasura/graphql-engine/tree/stable/community/sample-apps/realtime-poll)
  - [Εξερευνήστε APIs](https://realtime-poll.demo.hasura.app/console)

### Videos

* [Προσθέστε το GraphQL σε μια self-hosted GitLab](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Εφαρμογή λίστας εργασιών με Auth0 και back-end GraphQL](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GraphQL API στο GitLab ενσωματωμένο με έλεγχο ταυτότητας GitLab](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [Πίνακας ελέγχου για 10 εκατομμύρια διαδρομές γεωγραφικού εντοπισμού (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA)(*3:06 mins*)

## Υποστήριξη & Αντιμετώπιση προβλημάτων

Τα έγγραφα και η κοινότητα θα σας βοηθήσουν να λύσετε τα περισσότερα προβλήματα. Εάν αντιμετωπίσατε κάποιο σφάλμα ή πρέπει να επικοινωνήσετε μαζί μας, μπορείτε να το κάνετε μέσω των παρακάτω καναλιών:

* Support & feedback: [Discord](https://discord.gg/vBPpJkS)
* Εντοπισμός προβλημάτων και σφαλμάτων: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Ακολουθήστε τις ενημερωσεις για τα προϊόντα μας: [@HasuraHQ](https://twitter.com/hasurahq)
* Συνομιλήστε μαζί μας στο [website chat](https://hasura.io)

Συμμετέχουμε στην ανάπτυξη ενός ανοιχτού και φιλόξενου περιβάλλοντος στην κοινότητα. Συμβουλευτείτε τον [Κώδικα Συμπεριφοράς](https://github.com/hasura/graphql-engine/blob/stable/code-of-conduct.md).

Αν θέλετε να αναφέρετε ένα πρόβλημα ασφαλείας, παρακαλούμε [διαβάστε αυτό](https://github.com/hasura/graphql-engine/blob/stable/SECURITY.md).

## Συνεισφορά

Δείτε τον [οδηγό συνεισφοράς](https://github.com/hasura/graphql-engine/blob/stable/CONTRIBUTING.md) για περισσότερες λεπτομέρειες.

## Στοιχεία επωνυμίας

Τα στοιχεία μάρκας Hasura (λογότυπα, μασκότ Hasura, "powered by" εμβλήματα κ.λπ.) μπορούν να βρεθούν στον κατάλογο περιουσιακών στοιχείων / μάρκας. Είστε ελεύθεροι να τα χρησιμοποιήσετε στην εφαρμογή / ιστοσελίδα σας κτλ ... Θα χαρούμε να προσθέσετε το σήμα "Powered by Hasura" στην εφαρμογή που αναπτύξατε με την Hasura. ❤️

<div style="display: flex;">
  <img src="https://github.com/hasura/graphql-engine/blob/stable/assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="https://github.com/hasura/graphql-engine/blob/stable/assets/brand/powered_by_hasura_white.svg" width="150px"/>
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

## Άδεια

Η κεντρική GraphQL Engine είναι διαθέσιμη κάτω από την [Άδεια Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Όλα τα **άλλα περιεχόμενα** (εκτός από εκείνα σε [`server`](server), [`cli`](cli) και
[`console`](console) καταλόγους) είναι διαθέσιμα κάτω από την [Άδεια MIT](https://github.com/hasura/graphql-engine/blob/stable/LICENSE-community).
Αυτό περιλαμβάνει τα πάντα σε [`docs`](docs) και [`community`](community)
καταλόγους.
