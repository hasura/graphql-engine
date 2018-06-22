Run Hasura GraphQL Engine on Heroku (Simple)
============================================

This guide will help you get Hasura GraphQL engine and Postgres running on **Heroku's free tier**.
It is the easiest and fastest way of trying Hasura out if you don't have Docker!

Step 1: Deploy to Heroku & open your Heroku app
-----------------------------------------------

Deploy to Heroku by clicking:

.. image:: https://camo.githubusercontent.com/83b0e95b38892b49184e07ad572c94c8038323fb/68747470733a2f2f7777772e6865726f6b7563646e2e636f6d2f6465706c6f792f627574746f6e2e737667
  :width: 200px
  :alt: heroku_deploy_button
  :class: no-shadow
  :target: https://heroku.com/deploy?template=https://github.com/karthikvt26/heroku-push

**Note:** You might need to sign up on Heroku. You won't need a credit card, and once you sign up you'll be redirected to your heroku app creation page.

.. image:: ../../../img/graphql/manual/getting-started/heroku-app.png

Note that **Heroku's free Postgres add-on** is also automatically provisioned!

That's it!  Head to https://YOUR_HEROKU_APP.herokuapp.com and open your app.
You should see the Hasura GraphQL engine landing page!

.. image:: ../../../img/graphql/manual/getting-started/heroku-app-deployed.png

Step 2: Download the Hasura CLI
------------------------------------------------------

Follow the instructions to download the Hasura CLI.

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


Step 3: Setup a Hasura project and open the console
---------------------------------------------------

Run the following commands which will result in having the Hasura console up:

.. code-block:: none

   hasura-dev init --directory my-project --endpoint https://YOUR_HEROKU_APP.herokuapp.com/
   cd my-project
   hasura-dev console

Your browser window should open up automatically at http://localhost:9695 with your Hasura console pointing to the
Heroku app where Hasura GraphQL engine is running!

.. image:: ../../../img/graphql/manual/getting-started/console.png

Next: Make your first GraphQL query!
------------------------------------

Next, make your :doc:`first graphql query<first-graphql-query>`.
