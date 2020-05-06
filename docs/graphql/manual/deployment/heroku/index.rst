.. meta::
   :description: Deploy Hasura GraphQL engine with Heroku
   :keywords: hasura, docs, deployment, heroku

.. _deploy_heroku:

Run Hasura GraphQL engine on Heroku
===================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

This guide will help you get the Hasura GraphQL engine running as a "git push to deploy" app on
`Heroku <https://www.heroku.com/platform>`_ and connecting it to a `Heroku Postgres <https://www.heroku.com/postgres>`_
instance. If you want a simple, quick deployment on Heroku, follow this :ref:`Heroku quickstart
guide <heroku_simple>`.

Clone the Hasura GraphQL engine Heroku app
------------------------------------------

The Hasura app with Heroku buildpack/configuration is available at:

https://github.com/hasura/graphql-engine-heroku

Configure database URL
^^^^^^^^^^^^^^^^^^^^^^

Edit the command in the ``Dockerfile`` to change which database the Hasura GraphQL engine connects to.
By default, it connects to the primary database in your app which is available at ``DATABASE_URL``.

.. code-block:: dockerfile
   :emphasize-lines: 6

   FROM hasura/graphql-engine:latest

   # Change $DATABASE_URL to your Heroku Postgres URL if you're not using
   # the primary Postgres instance in your app
   CMD graphql-engine \
     --database-url $DATABASE_URL \
     serve \
     --server-port $PORT \
     --enable-console

Read about more configuration options :ref:`here <server_flag_reference>`.

.. note::

  Hasura GraphQL engine needs access permissions to your Postgres database as described in
  :ref:`Postgres permissions <postgres_permissions>`.


Deploying
---------

These are some sample deployment instructions while creating a new app.

Step 1: Create an app with **--stack=container**
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the `Heroku CLI <https://devcenter.heroku.com/articles/heroku-cli>`_ to create a new Heroku app. Let's call
the app ``graphql-on-postgres``.

.. code-block:: bash

  # Replace graphql-on-postgres with whatever you'd like your app to be called
  $ heroku create graphql-on-postgres --stack=container

  Creating ⬢ graphql-on-postgres... done, stack is container
  https://graphql-on-postgres.herokuapp.com/ | https://git.heroku.com/graphql-on-postgres.git

**Note**:

- ``HEROKU_GIT_REMOTE``: `https://git.heroku.com/graphql-on-postgres.git`
- ``HEROKU_APP_URL``: `https://graphql-on-postgres.herokuapp.com/`

Step 2: Create the Heroku Postgres add-on
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create the Postgres add-on in your Heroku app.

.. code-block:: bash

  $ heroku addons:create heroku-postgresql:hobby-dev -a graphql-on-postgres

  Creating heroku-postgresql:hobby-dev on ⬢ graphql-on-postgres... free
  Database has been created and is available
   ! This database is empty. If upgrading, you can transfer
    ! data from another database with pg:copy
    Created postgresql-angular-20334 as DATABASE_URL
    Use heroku addons:docs heroku-postgresql to view documentation

Step 3: **git push** to deploy
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Remember to change ``HEROKU_GIT_REMOTE`` to your git remote below. In our case:
``https://git.heroku.com/graphql-on-postgres.git``.

.. code-block:: bash

  $ git init && git add .
  $ git commit -m "first commit"
  $ git remote add heroku HEROKU_GIT_REMOTE
  $ git push heroku master

Visit ``https://graphql-on-postgres.herokuapp.com`` (replace ``graphql-on-postgres`` with your app name) and
you should see the Hasura console.

Advanced
--------

- :ref:`Securing your GraphQL endpoint <heroku_secure>`
- :ref:`heroku_existing_db`
- :ref:`GraphQL engine server logs <heroku_logs>`
- :ref:`Updating GraphQL engine <heroku_update>`
- :ref:`Setting up migrations <auth>`

.. toctree::
   :titlesonly:
   :hidden:

   Securing your GraphQL endpoint <securing-graphql-endpoint>
   using-existing-heroku-database
   GraphQL engine server logs <logging>
   Updating GraphQL engine <updating>
