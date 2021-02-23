.. meta::
   :description: Automatically sync database URL of a Heroku Postgres into a Hasura Cloud project
   :keywords: hasura, docs, project, team, heroku, database url, sync

.. _heroku_database_url_sync:

Heroku database URL sync
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Hasura Cloud can keep your project's Heroku database URL i.e. the ``HEROKU_DATABASE_URL`` env var in sync with Postgres from a Heroku app.
This is especially helpful in cases when the database credentials of Heroku Postgres are rotated automatically by Heroku.

.. note::
   
   The database sync automatically gets disconnected if you transfer the ownership of your project to another account. The new project owner
   can restart the sync with the Heroku account that they have connected their Hasura Cloud account to.

Enable Heroku database URL sync
-------------------------------

If you create a project with a Heroku trial database using the Hasura console, your project has the Heroku database URL sync enabled by
default, which means, Hasura Cloud keeps the database URL of your project in sync with the related Heroku Postgres.

Opt out
-------

If your project has Heroku database URL sync enabled, you can opt out as follows:

1. Go to the ``Env vars`` tab of your project and click on the ``HEROKU_DATABASE_URL`` env var.

2. Click on ``Opt out of the sync`` button next to Heroku note.

   .. thumbnail:: /img/graphql/cloud/projects/heroku-db-sync-enabled.png
      :alt: Add collaborator
      :width: 1100px

How it works?
-------------

Heroku database URL sync is useful because Postgres credentials of Heroku Postgres are sometimes rotated thus making the old database
URL invalid. Hasura Cloud listens to the changes in the database URL of your app and keeps the project updated. This is done using:

1. `Heroku Releases <https://devcenter.heroku.com/articles/releases>`__: Whenever a config variable of a Heroku app changes, a new
   ``release`` is made for that app.
2. `Heroku Webhooks <https://devcenter.heroku.com/articles/app-webhooks>`__: Heroku allows us to get notifications about these releases on a webhook.

Whenever Postgres credentials of a Heroku app are rotated:

1. The ``DATABASE_URL`` config variable of the Heroku app gets updated with the new credentials.
2. The config variable change triggers a new release, which notifies Hasura Cloud's webhook.
3. When Hasura Cloud is notified about the new release, it fetches the newest database URL from Heroku and updates the
   ``HEROKU_DATABASE_URL`` env var of your project with it.
4. This way, your project is always configured with the correct database URL.
