# Evènenements déclencheurs sur Postgres

Déclenchez des webhooks à partir d'évènements de la base de données en utilisant les évènenements déclencheurs d'Hasura GraphQL Engine.

![Démo des évènenements déclencheurs](../assets/event-triggers.gif)

## Points clés

* **Développez des fonctionnalités/applications asynchrones & réactives**: Déclenchez des fonctions serverless ou cloud asynchrones afin
de réduire les coûts d'infrastructure et simlifier les DevOps pour les développeurs.

* **Atomiques & Fiables**: En utilisant les déclencheurs natifs de Postgres, tout action significative sur la base de données est capturée comme évènement. Même si Hasura est inactif ou en cours de mise à jour, les évènements seront capturés et acheminés dés que possible avec une garantie de livraison *atleast-once*. Vous pouvez même configurer les politiques d'acheminement telles que `max_retries` et `retry_interval`.

* **Evolutifs**: Le système d'évènement déclencheur peut être mis à l'échelle horizontallement - si vous avez besoin de traiter plus d'évènements, il vous suffit de lui fournir plus de ressources!

* **Fonctionnent avec des bases de données existantes, en production**: Faites le pointer vers une base de données Postgres existante pour instantanément écouter les changements de donnée et invoquer des webhooks.

* **Prêts pour l'observabilité & le monitoring**: Les évènements générés sont automatiquement instrumentalisés avec un `event-id`, et les logs structurés émis par Hasura facilitent l'usage de vos outils favoris pour exécuter un backend basé sur les évènements en production.
([regardez](https://youtu.be/WOPA52r3bzU) un apperçu utilisant [Honeycomb](https://honeycomb.io/)).

## Démarrage rapide:

### Déploiement en un click sur Heroku

La manière la plus rapide de tester les évènements déclencheurs est via Heroku.

1. Cliquez sur le bouton suivant pour déployer GraphQL Engine sur Heroku avec l'add-on Postgres gratuit:

    [![Déployer sur Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Ouvrez la consule Hasura

   Visitez `https://<app-name>.herokuapp.com` (*remplacez \<app-name\> avec le nom de votre application*) pour ouvrir la console d'administration.

3. Configurez votre premier évènement déclencheur et webhook

   Créez une table, configurez le changement de donnée que vous souhaitez utiliser comme déclencheur et invoquez immédiatement un webhook en ajoutant une nouvelle ligne dans votre table.
   Suivez ce [simple guide](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-event-trigger.html).

### Autres méthodes de déploiement

Pour les déploiements basés sur Docker et le options de configuration avancées, consultez les [guides de déploiement](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html).

### Demo (*30 secondes*)

[![Créez un évènement déclencheur et un webhook en 60 secondes](https://img.youtube.com/vi/EaTUVWnDCvA/0.jpg)](https://www.youtube.com/watch?v=EaTUVWnDCvA)

[Créez un évènement déclencheur et un webhook en 60 secondes](https://youtu.be/EaTUVWnDCvA)

### Kits de démarrage Serverless

Utilisez l'un des [kits de démarrage déclencheur serverless](../community/boilerplates/serverless-triggers) pour déployer un webhook pouvant capturer des évènements de base de données. Plateformes Serverless/cloud-functions couvertes par les kits de démarrage:

* [AWS Lambda](../community/boilerplates/serverless-triggers/aws-lambda)
* [Google Cloud Functions](../community/boilerplates/serverless-triggers/google-cloud-functions)
* [Azure Functions](../community/boilerplates/serverless-triggers/azure-functions)
* [Zeit Now](../community/boilerplates/serverless-triggers/zeit-now)

## Architecture

![Architecture des évènements déclencheurs](../assets/event-triggers-arch.png)

## Démos & Tutoriels: Développer des applications/fonctionnalités réactives & asynchrones

### Notifications

Déclenchez des notifications push et des emails à partir d'évènements de la base de données. Essayez la démo et les tutoriels ci-dessous pour voir comment les notifications push du navigateur sont déclenchées lorsque l'utilisateur insère de la donnée:

* [Regardez la démo](https://www.youtube.com/watch?v=nuSHkzE2-zo)
* [Essayez la](https://serverless-push.demo.hasura.app/)
* [Tutoriel](../community/examples/serverless-push)


<!--
### Logique métier asynchrone

Convertissez de la logique métier complexe, à exécution prolongée en logique basés sur les évènements, asynchrone et tolérante aux pannes. Essayez cette démo et le tutoriel ci-dessous pour voir comment une tâche de traitement d'image est exécutée de manière asynchrone pour convertir une image en version noir et blanche.

* [Regardez la démo](https://some-youtube-demo.com) (*10:00 mins*)
* [Essayez la](https://some-link/)
* [Tutoriel](https://some-other-link)

-->

### Transformation de donnée (ETL)

Transformez et chargez de la donnée dans des data-stores externes. Allez voir cette démo et le tutoriel ci-dessous pour voir comment la donnée Postgres est transformée pour constuire et remplir un index Algolia:

* [Regardez la démo](https://youtu.be/kWVEBWdEVAA)
* [Essayez la](https://serverless-etl.demo.hasura.app/)
* [Tutoriel](../community/examples/serverless-etl)

### Construction d'UX réactive pour votre backend asynchrone avec une API GraphQL temps-réel

Propagez de l'information asynchrone et évènementielle à l'UI des clients facilement avec les subscriptions GraphQL & les live-queries.

![Artichecture d'applications réactives](../assets/reactive-apps-arch.png)

**Regardez**: [Construire des applications réactivez avec un backend asynchrones](https://youtu.be/kTSOxRrtCeI) (*04:15 mins*)
