# Contribuer à Hasura graphql-engine

*Premièrement*: si vous n'êtes pas certain ou avez des craintes au sujet de quoi que ce soit,
il vous suffit de poser une question, ou de soumettre une issue ou pull request malgré tout.
Personne ne vous grondera alors que vous faites de votre mieux.
Le pire serait que l'on vous demande poliment de modifier quelque chose.
Nous apprécions toute contribution, et ne voulons pas qu'une muraille de règles entrave cela.

Cependant, pour ceux qui souhaitent un peu plus d'accompagnement sur la meilleure
manière de contribuer au projet, lisez plus loin. Ce document traite du sujet qui
vous intéresse. En répondant à tous les critères que nous recherchons, les chance que
nous puissions intégrer ou traiter vos contributions augmenteront.

## Vue d'ensemble

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) est un mono-repo
consistant en 3 composants. Chacun ayant son propre guide de contribution:

1. [Server (Haskell)](../server/CONTRIBUTING.md)
2. [CLI (Go)](../cli/CONTRIBUTING.md)
3. [Console (JavaScript)](../console/README.md#contributing-to-hasura-console)

Chacun de ces trois composants possède une version unique, désignée soit par le tag git, soit par une
combinaison du nom de la branche et du SHA de commit git.

Pour toutes les contributions, un CLC (Contat de Licence de Contributeur) doit être signé [ici](https://cla-assistant.io/hasura/graphql-engine) avant (ou après) que la pull request ait été soumise. Un bot demandera si nécessaire aux contributeurs de signer le CLC au moyen d'un commentaire de pull request.


### Documentation

Le guide de contribution pour la documentation peut être trouvé sur [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md).

### Contenu communautaire

Il n'existe aucun guide spécifique pour la contribution au contenu communautaire.
N'importe quoi pouvant aider la communauté ou les utilisateurs du GraphQL Engine
peut faire partie de cette section. Nous avons identifié [boilerplates](../community/boilerplates),
[sample-apps](../community/sample-apps) et [outils](../community/tools) comme candidats principaux.
N'hésitez pas à soumettre une pull request si vous avez quelque chose à ajouter (n'appartenant
pas forcément aux sections mentionnées ci-dessus).

## Problèmes

### Rapporter un Problème

- Soyez certains de tester avec la dernière version disponible. Il est possible que
  le bug que vous rencontrez ait déja été corrigé.

- Précisez les étapes à suivre pour reproduire le problème, en incluant la version
  de Postgres, la version de graphql-engine et la plateforme sur laquelle vous les
  exécutez (Heroku, Docker, etc ...).

- Veuillez inclure les logs du server, si nécessaire.


## Directives communes

- Soyez certain qu'il y a une issue associée avec le travail que vous êtes en train de réaliser.

- Si vous travaillez sur une issue, veuillez commenter ce que vous faites pour éviter une duplication
  du travail par d'autres.

- Veuillez "squasher" vos commits et ajouter une référence à l'issue en ajoutant `fix #<issue-no>`
  ou `close #<issue-no>` dans le message de commit, à la fin.
  Par exemple `resolve answers to everything (fix #42)` ou `resolve answers to everything, fix #42`

- Rebasez master avec votre branche avant de soumettre une pull request.

## Messages de commit

- La première ligne doit être un résumé des changements, ne dépassant pas 50 caractères, suivi
  par une section optionnelle contenant plus de détails au sujet des modifications.
  Référez vous à [ce lien](https://github.com/erlang/otp/wiki/writing-good-commit-messages)
  pour plus d'informations à propos de l'écriture de bons messages de commit.

- Utilisez l'impératif présent: "add/fix/change", et non "added/fixed/changed" ou "adds/fixes/changes".

- Ne mettez pas la première lettre de la ligne de résumé en majuscule.

- N'ajoutez pas de point (.) à la fin de la ligne de résumé.

(Credits: Certaines sections sont adaptées de https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
