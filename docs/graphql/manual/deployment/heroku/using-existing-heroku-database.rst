.. meta::
   :description: Use an existing database with Heroku deployment
   :keywords: hasura, docs, deployment, heroku, existing database

.. _heroku_existing_db:

Using an existing Heroku database
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Let's say you have an existing `Heroku Postgres <https://www.heroku.com/postgres>`__ database with data in it, and you'd
like add GraphQL on it.

.. note::

   In case you're exposing an existing database (esp. if it is production), please configure an admin secret key
   for the console and the GraphQL endpoint.

Step 1: Deploy Hasura on Heroku
-------------------------------

Deploy Hasura on Heroku by clicking on this button:

.. image:: https://camo.githubusercontent.com/83b0e95b38892b49184e07ad572c94c8038323fb/68747470733a2f2f7777772e6865726f6b7563646e2e636f6d2f6465706c6f792f627574746f6e2e737667
  :width: 200px
  :alt: heroku_deploy_button
  :class: no-shadow
  :target: https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku

Follow the Heroku instructions to deploy, check if the Hasura console loads up when you click on **View app** and then head
to the **Manage App** screen on your Heroku dashboard.

This will deploy Hasura with a free Postgres add-on automatically provisioned.

Step 2: Remove the created Postgres add-on in the app
-----------------------------------------------------

Head to your Heroku dashboard and delete the Postgres add-on created in the previous step:

.. thumbnail:: ../../../../img/graphql/manual/deployment/remove-heroku-postgres-addon.png
   :alt: Delete the Postgres add-on

Step 3: Configure environment variables
---------------------------------------

Now configure the ``DATABASE_URL`` with your existing Heroku Postgres database URL and a ``HASURA_GRAPHQL_ADMIN_SECRET``
if you want to secure your endpoint.

.. thumbnail:: ../../../../img/graphql/manual/deployment/heroku-database-url-access.png
   :alt: Configure environment variables

.. note::

  The Hasura GraphQL engine needs access permissions to your Postgres database as described in
  :ref:`Postgres permissions <postgres_permissions>`.

Step 4: Track tables and relationships
--------------------------------------

Wait for the GraphQL engine to restart and then see :ref:`schema_existing_db` to enable GraphQL
over the database.
