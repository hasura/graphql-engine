Run Hasura GraphQL Engine on Heroku
===================================

Prerequisites:
**************

To proceed with the installation, please ensure that you have an account with Heroku and you have a working setup of the following softwares

- `Heroku <https://devcenter.heroku.com/articles/heroku-cli#download-and-install>`_
- `Docker <https://docs.docker.com/install/>`_

Step 1: Setting up
******************

Create an application
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

  $ heroku create graphql-on-postgres --stack=container
  Creating ⬢ graphql-on-postgres... done, stack is container
  https://graphql-on-postgres.herokuapp.com/ | https://git.heroku.com/graphql-on-postgres.git

.. note:: 

  Note that the ``HEROKU_GIT_REMOTE`` url is ``https://git.heroku.com/graphql-on-postgres.git`` and the ``HEROKU_APP_URL`` url is ``https://graphql-on-postgres.herokuapp.com/``. We will use these in the coming sections below.

Create Heroku Postgres Addon
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

  $ heroku addons:create heroku-postgresql:hobby-dev -a graphql-on-postgres
  Creating heroku-postgresql:hobby-dev on ⬢ graphql-on-postgres... free
  Database has been created and is available
   ! This database is empty. If upgrading, you can transfer
    ! data from another database with pg:copy
    Created postgresql-angular-20334 as DATABASE_URL
    Use heroku addons:docs heroku-postgresql to view documentation

The above command creates a free version of postgres and it is run against an app ``graphql-on-postgres``, You can choose from the list of postgres plans `here <https://www.heroku.com/pricing#postgres-pricing>`_

Step 2: Initialize a project directory
**************************************

.. code-block:: bash

  hasura-dev init --directory my-project

Step 3: Deploy to heroku
************************

.. code-block:: bash

  cd my-project
  cd __install/heroku
  git init && git add .
  git commit -am 'first commit'
  git remote add heroku HEROKU_GIT_REMOTE
  git push heroku master

Please visit `https://graphql-on-postgres.herokuapp.com <https://graphql-on-postgres.herokuapp.com>`_ (Please note to replace ``graphql-on-postgres`` with your app name) and you should see the page as in the screenshot below.

.. image:: ../../../../img/InstallSuccess.jpg
  :alt: Heroku installation success


Step 4: Open the hasura console
*******************************

In the my-project/config.yaml file set the endpoint:

.. code-block:: bash

  endpoint: HEROKU_APP_URL

Now, open the hasura console:

.. code-block:: bash

  # Run this command in the my-project/ directory
  $ hasura-dev console

Checkout our :doc:`schema <../../schema/index>` section to know more about how to create tables, interact with them.
