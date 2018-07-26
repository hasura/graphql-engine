Run Hasura GraphQL Engine on Heroku
===================================

This guide will help you get Hasura GraphQL engine running as a “git push to deploy” app on Heroku and connecting it
to a Heroku Postgres instance. If you want a simple, quick deployment on Heroku, follow this
:doc:`Heroku quickstart guide <../../getting-started/heroku-simple>`.

Clone the Hasura GraphQL engine Heroku app
------------------------------------------

The Hasura app with Heroku buildpack/configuration is available at:
https://github.com/hasura/graphql-engine-heroku

DATABASE_URL settings
^^^^^^^^^^^^^^^^^^^^^

Edit the command in the ``Dockerfile`` to change which database Hasura GraphQL engine connects to.
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

Read about more configuration options :doc:`here <../graphql-engine-flags/reference>`.

Deploying
---------

These are some sample deployment instructions while creating a new app.

Step 1: Create app with ``--stack=container``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We're creating an app named ``graphql-on-postgres``.
You should change this name to whatever you'd like your app to be called.

.. code-block:: none

  $ heroku create graphql-on-postgres --stack=container

  Creating ⬢ graphql-on-postgres... done, stack is container
  https://graphql-on-postgres.herokuapp.com/ | https://git.heroku.com/graphql-on-postgres.git

**Note**:

- ``HEROKU_GIT_REMOTE``: ``https://git.heroku.com/graphql-on-postgres.git``
- ``HEROKU_APP_URL``: ``https://graphql-on-postgres.herokuapp.com/``

Step 2: Create the Heroku Postgres Addon
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create the Postgres addon in your Heroku app.

.. code-block:: none

  $ heroku addons:create heroku-postgresql:hobby-dev -a graphql-on-postgres

  Creating heroku-postgresql:hobby-dev on ⬢ graphql-on-postgres... free
  Database has been created and is available
   ! This database is empty. If upgrading, you can transfer
    ! data from another database with pg:copy
    Created postgresql-angular-20334 as DATABASE_URL
    Use heroku addons:docs heroku-postgresql to view documentation

Step 3: git push to deploy
^^^^^^^^^^^^^^^^^^^^^^^^^^
Remember to change HEROKU_GIT_REMOTE to your git remote below. In our case:
``https://git.heroku.com/graphql-on-postgres.git``

.. code-block:: bash

  git init && git add .
  git commit -am 'first commit'
  git remote add heroku HEROKU_GIT_REMOTE
  git push heroku master

Visit ``https://graphql-on-postgres.herokuapp.com`` (replace ``graphql-on-postgres`` with your app name) and
you should see the Hasura console.

Advanced:
---------

.. toctree::
   :titlesonly:

   Securing your GraphQL endpoint <securing-graphql-endpoint>
   using-existing-heroku-database
   Updating GraphQL engine <updating>
