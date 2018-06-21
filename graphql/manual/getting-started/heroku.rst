Run Hasura GraphQL Engine on Heroku
===================================

This guide will help you get Hasura running as a "git push" to deploy app on Heroku.
This is recommended for users that already know how to run apps and postgres on Heroku.

Clone the Hasura GraphQL engine heroku app
----------------------------------------

The Hasura app with Heroku buildpack/configuration is available at:
https://github.com/hasura/graphql-engine-heroku

Change the dyno and postgres configurations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Open ``app.json`` to edit the dyno and postgres addon sizes.

.. code-block:: json
   :emphasize-lines: 13,14,20

   {
     "name": "Hasura GraphQL Engine",
     "description": "Hasura GraphQL Engine <> heroku",
     "keywords": [
       "graphql",
       "heroku",
       "postgres",
       "hasura"
     ],
     "repository": "https://github.com/hasura/graphql-engine-heroku",
     "formation": {
       "web": {
         "quantity": 1,
         "size": "free"
       }
     },
     "stack": "container",
     "addons": [
       {
         "plan": "heroku-postgresql:hobby-dev"
       }
     ]
   }

DATABASE_URL settings
^^^^^^^^^^^^^^^^^^^^^

Edit the command in the ``Dockerfile`` to change which database Hasura connects to.
By default, it connects to the primary database in your app which is available at ``DATABASE_URL``.

.. code-block:: Dockerfile
   :emphasize-lines: 6

   FROM hasura/graphql-engine:latest

   # Change $DATABASE_URL to your heroku postgres URL if you're not using
   # the primary postgres instance in your app
   CMD graphql-engine \
       --database-url $DATABASE_URL \
       serve \
       --server-port $PORT


Read about more configuration options :doc:`here<../deployment/options>`.

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

- ``HEROKU_GIT_REMOTE``: https://git.heroku.com/graphql-on-postgres.git
- ``HEROKU_APP_URL``: https://graphql-on-postgres.herokuapp.com/

Step 2: Create the Heroku Postgres Addon
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create the postgres addon in your heroku app.

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
Remember to change HEROKU_GIT_REMOTE to your git remote below. In our case: https://git.heroku.com/graphql-on-postgres.git

.. code-block:: bash

  git init && git add .
  git commit -am 'first commit'
  git remote add heroku HEROKU_GIT_REMOTE
  git push heroku master

Visit `https://graphql-on-postgres.herokuapp.com <https://graphql-on-postgres.herokuapp.com>`_ (Please note to replace ``graphql-on-postgres`` with your app name) and you should see the page below.

.. image:: ../../../img/InstallSuccess.jpg
  :alt: Heroku installation success

Initliase a Hasura project
--------------------------
Now that we have Hasura and postgres running, let's open up the Hasura console and start making GraphQL queries!

Install the Hasura CLI
^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Mac

      In your terminal enter the following command:

      .. code-block:: bash

        curl -L https://storage.googleapis.com/hasuractl/install-dev.sh | bash

      As this is a preview release of the Hasura CLI, the CLI is named ``hasura-dev`` and not ``hasura``.

   .. tab:: Linux

      Open your linux shell and run the following command:

      .. code-block:: bash

        curl -L https://storage.googleapis.com/hasuractl/install-dev.sh | bash

      As this is a preview release of the Hasura CLI, the CLI is named ``hasura-dev`` and not ``hasura``.

   .. tab:: Windows

       Coming soon ...


Initlialise the Hasura project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Replace ``HEROKU_APP_URL`` with your apps URL. In our case, https://graphql-on-postgres.herokuapp.com.

.. code-block:: bash

  hasura-dev init --directory my-project --endpoint HEROKU_APP_URL


Step 4: Open the hasura console
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

  # Run this command in the my-project/ directory
  $ cd my-project
  $ hasura-dev console


Next: Make your first GraphQL query!
------------------------------------

Next, make your :doc:`first graphql query<first-graphql-query>`.
