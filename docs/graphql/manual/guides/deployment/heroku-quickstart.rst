.. meta::
   :description: Get started with Hasura using Heroku
   :keywords: hasura, docs, start, heroku

.. _heroku_quickstart:

One-click deployment with Heroku
================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide will help you get the Hasura GraphQL engine and Postgres running on `Heroku's free tier <https://www.heroku.com/free>`_.
It is the easiest and fastest way of trying out Hasura.

If you'd like to link this to an existing database, please head to this guide instead:
:ref:`Using an existing database on Heroku <heroku_existing_db>`.

Deploy to Heroku
----------------

Click the button below to deploy to Heroku:

.. image:: https://camo.githubusercontent.com/83b0e95b38892b49184e07ad572c94c8038323fb/68747470733a2f2f7777772e6865726f6b7563646e2e636f6d2f6465706c6f792f627574746f6e2e737667
  :width: 200px
  :alt: heroku_deploy_button
  :class: no-shadow
  :target: https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku

.. note::
   If you don't have an account on Heroku, you need to sign up on Heroku. You won't need a credit card, and once you
   sign up you'll be redirected to your Heroku app creation page automatically.

.. thumbnail:: /img/graphql/manual/guides/heroku-app.png
   :alt: Deploy to Heroku 

Note that **Heroku's free Postgres add-on** is also automatically provisioned!

Open the Hasura console
-----------------------

That's it!  Head to ``https://<YOUR_HEROKU_APP>.herokuapp.com`` and open your app.
You should see the Hasura console.

.. thumbnail:: /img/graphql/manual/guides/heroku-app-deployed.png
   :alt: Open the Hasura console

Hello World (GraphQL or event triggers)
---------------------------------------

Make your :ref:`first graphql query <first_graphql_query>`

OR

Set up your :ref:`first event trigger <first_event_trigger>`

Advanced
--------

This was a quickstart guide to get the Hasura GraphQL engine up and running quickly. For more detailed instructions
on deploying using Heroku, check out :ref:`deploy_heroku`.
