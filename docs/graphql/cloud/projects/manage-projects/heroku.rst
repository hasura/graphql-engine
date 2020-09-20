.. meta::
   :description: Managing teams on Hasura Cloud
   :keywords: hasura, docs, project, team, heroku, database url, sync

.. _heroku_database_url_sync:

Heroku database URL sync
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Hasura cloud can keep your project's database URL i.e. the HASURA_GRAPHQL_DATABASE_URL env var in sync with Postgres from a Heroku app. This is especially helpful in cases when the database credentials of Heroku Postgres are rotated.

If you create a project with a Heroku trial database using the cloud dashboard, your project has the Heroku database URL sync enabled by default, which means, Hasura Cloud keeps the database URL of your project in sync with the related Heroku Postgres.

.. admonition::

   Note: Heroku Database URL sync was not automatically enabled for projects created before the the launch of this feature (Sep 19, 2020). If you wish to enable it, please do it manually.


Enable Heroku database URL Sync
--------------------------------

If you want to enable database URL sync between your Hasura cloud project and an existing Heroku app, you can do it as follows:

1. Go to the `Env vars` tab of your project and click on the ``HASURA_GRAPHQL_DATABASE_URL`` env var.

2. Click on the ``Sync Database URL`` button.

.. thumbnail:: /img/graphql/cloud/projects/heroku-db-sync-disabled.png
   :alt: Add collaborator
   :width: 865px


3. If you are not logged into Heroku, you can do it by clicking on the Heroku button.

4. Once you login, you will see a list of your Heroku apps. You can choose the app that you wish to start database URL sync with.

.. thumbnail:: /img/graphql/cloud/projects/heroku-db-sync-choose.png
   :alt: Add collaborator
   :width: 865px



Opt out
-------

If your project has Heroku database URL sync enabled, you can opt out as follows:

1. Go to the `Env vars` tab of your project and click on the ``HASURA_GRAPHQL_DATABASE_URL`` env var.

2. Click on ``Opt out of the sync`` button next to Heroku note.

.. thumbnail:: /img/graphql/cloud/projects/heroku-db-sync-enabled.png
   :alt: Add collaborator
   :width: 865px


How it works?
-------------

Heroku database URL sync is useful because postgres credentials of Heroku postgres are sometimes rotated thus making the old database URL invalid. Hasura Cloud listens to the changes in the database URL of your app and keeps the project updated. This is done using:

1. `Heroku Releases <https://devcenter.heroku.com/articles/releases>`__: Whenever a config variable of a Heroku app changes, a new ``release`` is made for that app.
2. `Heroku Webhooks <https://devcenter.heroku.com/articles/app-webhooks>`__: Heroku allows us to get notifications about these releases on a webhook.


Whenever postgres credentials of a Heroku app are rotated:

1. The ``DATABASE_URL`` config variable of the Heroku app gets updated with the new credentials.
2. The config variable change triggers a new release, which notifies Hasura Cloud's webhook.
3. When Hasura Cloud is notified about the new release, it fetches the newest database URL from Heroku and updates the `HASURA_GRAPHQL_DATABASE_URL` env var of your project with it.
4. This way, your project is always configured with the correct database URL