# Contribuire ad Hasura graphql-engine

_Prima di tutto_: se non ti senti sicuro/a su come iniziare a contribuire, chiedi pure sul nostro [canale Discord](https://discordapp.com/invite/hasura) nel canale #contrib. Puoi anche iniziare immediatamente con il tuo contributo e ti daremo feedback. Non ti preoccupare: il peggio che può capitare è che ti chiederemo gentilmente di apportare delle modifiche. Siamo grati per ogni contributo, e non vogliamo che una barriera di regole si metta di mezzo.

In ogni caso, per chi volesse un po' più di aiuto sul modo migliore per contribuire al progetto, basta procedere con la lettura. Questo documento copre esattamente quello che cerchiamo in un contributo. Tenendo conto di tutti i punti di seguito, le possibilità che possiamo mergiare rapidamente o almeno prendere in carico il tuo contributo cresceranno sensibilmente.

## Indice

[1. Codice di condotta ](#code-of-conduct)

[2. Overview del repository ](#overview)

[3. I contributor al primo contributo sono benvenuti! ](#first-timers)

[4. Aree dove contribuire ](#areas)

[5. Modi di contribuire ](#ways)

[6. Messaggi di commit ](#commit-messages)

[7. Traduzioni ](#translations)

<a name="code-of-conduct"></a>

## 1. Codice di condotta

Per favore segui il nostro [codice di condotta](code-of-conduct.md) nel contesto di ogni contributo che fai per Hasura.

<a name="overview"></a>

## 2. Overview del repository

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) è un mono-repo che consiste di tre componenti.
Ognuno ha la sua guida per contribuire:

1. [Server (Haskell)](server/CONTRIBUTING.md)

2. [CLI (Go)](cli/CONTRIBUTING.md)

3. [Console (JavaScript)](console/README.md#contributing-to-hasura-console)

Ognuno dei tre componenti ha una versione, indicata dal tag git o da una combinazione del nome del branch e dello SHA del commit.

Per tutti i contributi, un CLA (Contributor License Agreement) deve essere firmato [qui](https://cla-assistant.io/hasura/graphql-engine) prima (o dopo) che la pull request è stata inviata.
Un bot inviterà i contributor a firmare il CLA tramite un commento alla pull request, se necessario.

<a name="first-timers"></a>

## 3. I contributor al primo contributo sono benvenuti!

Apprezziamo molto i contributor al loro primo contributo e siamo felici di assisterti nell'iniziare. Se hai domande, mettiti in contatto con noi!

Puoi trovare tutti gli issue adatti ai primi contributi  [qui](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

<a name="areas"></a>

## 4. Aree dove contribuire

Ovviamente, siamo grati per i contributi in tutti i componenti di Hasura. Tuttavia, abbiamo identificato tre aree che sono particolarmente adatte per i contributi open source:

### Documentazione

Il nostro obiettivo è mantenere la documentazione esaustiva e aggiornata. Se vuoi aiutarci a farlo, siamo grati per ogni tipo di contributo:

- Segnalazioni di contenuto mancante

- Correzione di errori nella documentazione esistente

- Aiuto nell'aggiunta di documentazione

La guida ai contributi per la documentazione si trova [qui](docs/CONTRIBUTING.md).

### Contenuto per la community

Da quando abbiamo lanciato la nostra [pagina di risorse per imparare](https://hasura.io/learn/), siamo grati per i contributi in:

- Correzione di errori in tutorial esistenti

- Aggiunta di nuovi tutorial (per favore mettiti in contatto se hai idee su come evitare la duplicazione di contenuti)

Il README del repository del contenuto per la community si trova [qui](https://github.com/hasura/learn-graphql).

Oltre al contenuto della sezione "learn", abbiamo identificato tre altri modi di contribuire con del contenuto tecnico da parte della community:

- [Boilerplates](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [Sample apps](https://github.com/hasura/graphql-engine/tree/master/community/sample-apps)

- [Strumenti](community/tools)

Se vuoi contribuire alla community tramite

- scrittura di un blogpost

- intervento a un evento

- organizzazione di un workshop

controlla la nostra [community wiki](https://github.com/hasura/graphql-engine/wiki/Community-Wiki).

Invia pure una pull request se hai qualcosa da aggiungere anche se non fa parte di quanto menzionato sopra.

### Hasura CLI

Abbiamo alcuni issue sulla CLI che sono adatti ai contributi open source. Se conosci Go o vorresti impararlo attivamente, questi [issues](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22) sono adatti.

Il README del repository della CLI si trova [qui](https://github.com/hasura/graphql-engine/tree/master/cli).

<a name="ways"></a>

## 5. Modi di contribuire

### Aprire un Issue

- Accertati di aver testato con l'ultima versione rilasciata. È possibile che abbiamo già corretto il bug che stai verificando.

- Fornisci i passi per riprodurre il problema, inclusi la versione di Postgres, di graphql-engine e il provider su cui stai eseguendo l'applicazione (Heroku, Docker, etc.).

- Per favore includi i log del server, se rilevanti.

### Lavorare su un issue

- Usiamo il [workflow fork-and-branch](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- Per favore accertati che ci sia un issue associato al lavoro che stai facendo.

- Se stai lavorando su un issue, per favore commenta che lo stai facendo per evitare che altri facciano il tuo stesso lavoro.

- Fai lo squash dei tuoi commit e fai riferimento all'issue usando `fix #<issue-no>` o `close #<issue-no>` nel messaggio di commit, alla fine.
  Per esempio: `resolve answers to everything (fix #42)` o `resolve answers to everything, fix #42`

- Fai il rebase di master sul tuo branch prima di inviare una pull request.

<a name="commit-messages"></a>

## 6. Messaggi di commit

- La prima riga dovrebbe essere un riassunto delle modifiche, non oltre 50 caratteri, seguita da un messaggio opzionale con più dettagli sulle modifiche. Fai riferimento a [questo link](https://github.com/erlang/otp/wiki/writing-good-commit-messages)
  per maggiori informazioni sui messaggi di commit.

- Usa l'imperativo presente: "add/fix/change", non "added/fixed/changed" né "adds/fixes/changes".

- Non iniziare la prima riga con la lettera maiuscola.

- Non aggiungere un punto (.) alla fine della prima riga.

<a name="translations"></a>

## 7. Traduzioni

Questo documento è disponibile nelle seguenti traduzioni:

- [Inglese 🇬🇧](../CONTRIBUTING.md)
- [Francese 🇫🇷](./CONTRIBUTING.french.md)

(Credits: Some sections are adapted from https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
