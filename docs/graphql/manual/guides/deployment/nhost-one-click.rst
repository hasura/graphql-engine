.. meta::
   :description: One-Click Deploy for Hasura GraphQL Engine on Nhost
   :keywords: hasura, docs, guide, deployment, nhost, postgresql, storage, auth

.. _deploy_nhost:

Deploying Hasura GraphQL engine on Nhost
========================================

.. contents::
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide shows how to deploy the Hasura GraphQL engine on `Nhost <https://nhost.io>`__.

One-Click deploy on Nhost
-------------------------

.. note::
   Make sure to `create an Nhost account <https://nhost.io/register>`_ first. Nhost offers a 30-day trial with its starter plan.

Once you're logged into your Nhost account, click the button bellow to configure your project.

.. image:: https://nhost.io/images/create-project-button.png
   :width: 300px
   :class: no-shadow
   :alt: Deploy with Nhost

On the following page you can choose a plan and a location for your project:

.. image:: https://nhost.io/images/create-project-form.png
   :class: no-shadow
   :alt: Choose a plan and locatino for your project

The creation and provisioning of your backend should be ready in roughly 30 seconds. 
Your project has now a GraphQL API, a PostgreSQL database, user management, and storage for your files!

Access your Hasura console
--------------------------

Once your Nhost project is up and running, please go ahead and click on the ``Hasura`` option on the left menu.

.. image:: https://nhost.io/images/project-hasura.png
   :class: no-shadow
   :alt: Hasura console URL on Nhost

The Hasura console endpoint is highlighted in red and has the following structure:

.. code-block:: bash

   https://<hasura-slug>.nhost.app/

To test it, you can either create tables directly using the console, or add new users to your project clicking on ``Auth`` on the left menu.

There is also a ``GraphQL API`` option for you to test the API.
Check out :ref:`making your first GraphQL query <first_graphql_query>` for more information.

References
----------

- `Nhost Docs <https://docs.nhost.io/>`__
