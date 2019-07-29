Updating Hasura GraphQL engine on Heroku
========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide will help you update Hasura GraphQL engine running on Heroku. This guide assumes that you already have
Hasura GraphQL engine running on Heroku.

The current latest version is:

.. raw:: html

   <code>hasura/graphql-engine:<span class="latest-release-tag">latest</span></code>

Follow these steps to update Hasura GraphQL engine to the lastest version:

Step 1: Clone the Hasura GraphQL engine Heroku app
--------------------------------------------------

The Hasura app with Heroku buildpack/configuration is available at:
https://github.com/hasura/graphql-engine-heroku

Clone the above repository.

If you already have this, then pull the latest changes which will have the updated GraphQL engine docker image.

Step 2: Attach your Heroku app
------------------------------

Let's say your Heroku app is called ``hasura-heroku`` and is running on ``https://hasura-heroku.herokuapp.com``.

Navigate to your project directory, use the `Heroku CLI <https://devcenter.heroku.com/articles/heroku-cli>`_ to configure the git repo you cloned in Step 1
to be able to push to this app.

.. code-block:: bash

   # Replace <hasura-heroku> with your Heroku app's name
   $ heroku git:remote -a <hasura-heroku>
   $ heroku stack:set container -a <hasura-heroku>

You can find your Heroku git repo in your Heroku - Settings - Info - Heroku Git URL

Step 3: Git push to deploy the latest Hasura GraphQL engine
-----------------------------------------------------------

When you ``git push`` to deploy, the Heroku app will get updated with the latest changes:

.. code-block:: bash

   $ git push heroku master

Deploy a specific version of Hasura GraphQL engine
--------------------------------------------------

Head to the ``Dockerfile`` in the git repo you cloned in Step 1.
Change the ``FROM`` line to the specific version you want. A list of all releases can be found
at https://github.com/hasura/graphql-engine/releases

.. code-block:: Dockerfile
   :emphasize-lines: 1

   FROM hasura/graphql-engine:v1.0.0-alpha01

   ...
   ...

Change ``v1.0.0-alpha01`` to ``v1.0.0-alpha02`` for example, and then ``git push heroku master`` to deploy.
