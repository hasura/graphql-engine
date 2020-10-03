# Współtworzenie Hasura GraphQL Enigne

_Po pierwsze_: jeśli nie czujesz się pewnie, jak zacząć współtworzyć, możesz zapytać nas na naszym [kanale Discord](https://discordapp.com/invite/hasura) na kanale #contrib. Możesz też po prostu przekazać swój wkład, a my prześlemy Ci swoją opinię. Nie martw się - najgorsze, co może się zdarzyć, to grzeczna prośba o zmianę. Doceniamy każdy wkład i nie chcemy, aby mnóstwo zasad stało na drodze.

Jednak dla tych osób, które chcą uzyskać nieco więcej wskazówek na temat najlepszego sposobu wniesienia wkładu do projektu, czytaj dalej. Ten dokument obejmuje to, czego szukasz. Odnosząc się do poniższych punktów, szanse, że my
można szybko połączyć lub zająć się Twoim wkładem wzrośnie.

## Spis treści

[1. Kodeks postępowania](#code-of-conduct)

[2. Omówienie repozytorium](#overview)

[3. Witamy nowych autorów! ](#first-timers)

[4. Obszary do wnoszenia wkładu](#areas)

[5. Sposoby wnoszenia wkładu](#ways)

[6. Komunikaty o zatwierdzeniach](#commit-messages)

<a name="code-of-conduct"> </a>

## 1. Kodeks postępowania

Prosimy o przestrzeganie naszego [Kodeksu postępowania](code-of-conduct.polish.md) w sprawie wszelkich wkładów do Hasura GraphQL Enigne.

<a name="overview"> </a>

## 2. Przegląd repozytorium

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) to mono-repo składający się z 3 elementów. Każdy ma własne przewodniki pomocnicze:

1. [Serwer (Haskell)](../server/CONTRIBUTING.md)

2. [CLI (Go)](../cli/CONTRIBUTING.md)

3. [Console (JavaScript)](../console/README.md#contributing-to-hasura-console)

Wszystkie trzy komponenty mają jedną wersję, oznaczoną tagiem git lub kombinacją nazwy gałęzi i SHA commita git.

W przypadku wszystkich wkładów należy podpisać CLA (umowę licencyjną współtwórcy) [tutaj](https://cla-assistant.io/hasura/graphql-engine) przed (lub po) przesłaniu pull requesta. Bot poprosi o podpisanie CLA za pomocą komentarza pod pull requestem, jeśli to konieczne.

<a name="first-timers"> </a>

## 3. Pierwsi Wspóltwócy witamy!

Doceniamy pierwszych współtwórców i chętnie pomożemy w rozpoczęciu. W przypadku pytań skontaktuj się z nami!

Wszystkie problemy odpowiednie dla osób, które usczestniczą pierwszy raz znajduję się [tutaj](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22 ).

<a name="areas"> </a>

## 4. Obszary działalności

Oczywiście doceniamy wkład we wszystkie składniki Hasury. Jednak zidentyfikowaliśmy trzy obszary, które są szczególnie odpowiednie dla publikacji typu open source.

### Dokumentacja

Naszym celem jest, aby nasza dokumentacja była wyczerpująca i aktualna. Jeśli chcesz nam w tym pomóc, będziemy wdzięczni za wszelkiego rodzaju wkład:

- Zgłoś brakującą zawartość

- Napraw błędy w istniejących dokumentach

- Pomóż nam w dodawaniu do dokumentów

Przewodnik pomocniczy dotyczący dokumentów można znaleźć pod adresem [docs/CONTRIBUTING.md](../docs/CONTRIBUTING.md).

### Treści społeczności

Odkąd uruchomiliśmy naszą [stronę edukacyjną](https://hasura.io/learn/), cieszymy się z następujących wkładów:

- Naprawa błędów w istniejących samouczkach do nauki

- Dodanie nowych samouczków (skontaktuj się z nami, jeśli masz pomysły, aby uniknąć powielania)

README repozytorium Learn można znaleźć [tutaj](https://github.com/hasura/learn-graphql).

Oprócz treści edukacyjnych zidentyfikowaliśmy trzy inne sposoby współtworzenia treści technicznych społeczności:

- [Boilerplates](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [Przykładowe aplikacje](https://github.com/hasura/graphql-engine/tree/master/community/sample-apps)

- [Narzędzia](../community/tools)

Jeśli chcesz wnieść swój wkład do społeczności przez

- pisanie technicznego posta na blogu

- przemawianie na wydarzeniu

- organizacje warsztatów

sprawdź nasze [wiki społeczności](https://github.com/hasura/graphql-engine/wiki/Community-Wiki).

Możesz przesłać pull requesta, jeśli masz coś do dodania, nawet jeśli nie jest to związane z niczym wymienionym powyżej.

### Hasura CLI

Mamy pewne problemy z CLI, które są odpowiednie dla uczestników open source. Jeśli znasz Go lub chciałbyś się tego nauczyć w praktyce, zapoznaj się z następującymi [problemami](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22).

README repozytorium CLI można znaleźć [tutaj](https://github.com/hasura/graphql-engine/tree/master/cli).

<a name="ways"> </a>

## 5. Sposoby wnoszenia wkładu

### Zgłaszanie problemu

- Upewnij się, że wykonujesz testy w najnowszej wydanej wersji. Możliwe, że mogliśmy już naprawić napotkany błąd.

- Podaj kroki w celu odtworzenia problemu, w tym wersję Postgres,
  Wersja silnika graphql i dostawca, z którego korzystasz (Heroku, Docker itp.).

- W razie potrzeby dołącz dzienniki serwera.

### Praca nad problemem

- Używamy [fork-and-branch git workflow](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- Upewnij się, że występujący problem jest związany z wykonywaną pracą.

- Jeśli pracujesz nad problemem, skomentuj, że robisz to, aby zapobiec powielaniu pracy również przez innych.

- Zmniejsz swoje commity i odnieś się do problemu za pomocą `fix # <issue-no>` lub `close # <issue-no>` na końcu wiadomości commitu. Na przykład: `resolve answers to everything (fix #42)` lub `resolve answers to everything, fix #42`

- Użyj rebase master ze swoją gałęzią przed przesłaniem pull requesta.

<a name="commit-messages"> </a>

## 6. Wiadomości Commita

- Pierwsza linia powinna zawierać podsumowanie zmian, nie więcej niż 50
  znaków, po których następuje opcjonalna treść zawierająca więcej szczegółów na temat
  zmiany. Zobacz [ten link](https://github.com/erlang/otp/wiki/writing-good-commit-messages)
  aby uzyskać więcej informacji na temat pisania dobrych wiadomości o zmianach.

- Używaj imperatywnego czasu teraźniejszego: „add/fix/change”, a nie „added/fixed/changed” ani „adds/fixes/changes”.

- Nie zaczynaj od wielkiej litery w podsumowaniu.

- Nie dodawaj kropki / kropki (.) na końcu podsumowania.
