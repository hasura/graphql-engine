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

One-click deploy on Nhost
-------------------------

.. note::
   Make sure to `create an Nhost account <https://nhost.io/register>`__ first. Nhost offers a 30-day trial with its starter plan.

Once you're logged into your Nhost account, click the button bellow to configure your project.

.. thumbnail:: /img/graphql/core/deployment/create-project-nhost.png
   :alt: Create a project

On the following page you can choose a plan and a location for your project:

.. thumbnail:: /img/graphql/core/deployment/choose-a-plan.png
   :alt: Choose a plan and location for your project

The creation and provisioning of your backend should be ready in roughly 30 seconds. 
Your project has now a GraphQL API, a PostgreSQL database, user management, and storage for your files!

Access your Hasura console
--------------------------

Once your Nhost project is up and running, a dashboard will come up and you can click on the ``Hasura`` menu on the navigation bar.

.. thumbnail:: /img/graphql/core/deployment/nhost-dashboard.png
   :alt: Dashboard   

Once the ``Hasura`` menu has been clicked on, the console becomes visible:

.. thumbnail:: /img/graphql/core/deployment/nhost-hasura-console.png
   :alt: Nhost Hasura console  

To test it, you can either create tables directly using the console, or add new users to your project clicking on ``Auth`` on the navigation menu.

There is also a ``GraphQL API`` option for you to test the API.
Check out :ref:`making your first GraphQL query <first_graphql_query>` for more information.

References
----------

- `Nhost Docs <https://docs.nhost.io/>`__
