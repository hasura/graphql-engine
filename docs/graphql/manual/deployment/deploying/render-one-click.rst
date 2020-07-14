.. meta::
   :description: One-Click Deploy for Hasura GraphQL Engine on Render
   :keywords: hasura, docs, guide, deployment, render, postgresql

.. _deploy_render:

Deploying Hasura GraphQL engine on Render
=========================================

.. contents::
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide shows how to deploy the Hasura GraphQL engine on `Render <https://render.com>`__.

One-Click Deploy on Render
--------------------------

.. note::
   Make sure to `create a free Render account <https://render.com/register>`__ first.

Once you're logged into your Render account, click the button below to deploy Hasura and a
new managed PostgreSQL database wired up to your Hasura instance.

.. image:: https://render.com/images/deploy-to-render-button.svg
   :width: 200px
   :alt: render_deploy_button
   :class: no-shadow
   :target: https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql

You will see the Hasura web service and PostgreSQL instance to be created:

.. thumbnail:: /img/graphql/manual/deployment/deploy-to-render-hasura-iac.png
   :alt: Deploy To Render Hasura Page

That's it! Read on to configure your Hasura instance.

Access your Hasura Console
--------------------------

Once **Deploy to Render** succeeds, you can click through to your Hasura service page on Render's dashboard.

.. thumbnail:: /img/graphql/manual/deployment/deploy-to-render-hasura-header.png
   :alt: Render Hasura Header

You can monitor the deployment of the Hasura web service from the **Logs** tab. Once the service is up, use the link on the service page to access your Hasura console:

.. code-block:: bash

   https://<your-hasura-slug>.onrender.com/

You can create tables and test your GraphQL queries here.

Check out :ref:`making your first GraphQL query <first_graphql_query>` or :ref:`setting up your first event trigger <first_event_trigger>` for more.

References
----------

- `Render Hasura GraphQL on GitHub <https://github.com/render-examples/hasura-graphql>`_
- `Render Docs <https://render.com/docs>`_
