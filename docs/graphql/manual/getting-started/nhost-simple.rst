.. meta::
   :description: Get started with Hasura using Nhost
   :keywords: hasura, docs, start, nhost

Quickstart with Nhost
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide will help you get the Hasura GraphQL engine and Postgres running on `Nhost <https://www.nhost.io/pricing>`_.

Nhost is a managed backend service which includes the following:

-	Postgres

-	Hasura

-	Authentication

-	File storage

It is the a fast way to get Hasura-based apps into production.

.. note::
   Nhost is a paid service. You can sign up for a 30-day trial without a credit card, but after that there is a monthly fee. You can also self-host using the open-source version, available on GitHub: `Hasura Backend Plus <https://www.nhost.io/pricing>`_.

Create a project
----------------

Click the button below to create an account with Nhost:

.. image:: ../../../img/graphql/manual/getting-started/nhost-register-button.png
  :width: 200px
  :alt: nhost_register_button
  :class: no-shadow
  :target: https://nhost.io/register

Once you have created an account, begin by creating your first project:

.. thumbnail:: ../../../img/graphql/manual/getting-started/nhost-create-project.png
   :alt: Create your first project on Nhost

Now you are good to go! Your new project includes Postgres, the Hasura GraphQL engine, authentication and file storage.

Open the Hasura console
-----------------------

Open the Hasura console by selecting it in the dashboard menu, copying your admin secret, and clicking your console url:

.. thumbnail:: ../../../img/graphql/manual/getting-started/nhost-open-console.png
   :alt: Open the Hasura console

Now you should see the Hasura console:

.. thumbnail:: ../../../img/graphql/manual/getting-started/nhost-hasura-console.png
   :alt: The Hasura console

Hello World (GraphQL or event triggers)
---------------------------------------

Now you can start experiementing with Hasura:

Make your :doc:`first graphql query <first-graphql-query>`

OR

Set up your :doc:`first event trigger <first-event-trigger>`

Next steps
----------

This was a quickstart guide to get the Hasura GraphQL engine up and running quickly.

For instructions and tutorials on building Hasura-based apps on Nhost, check out the `Nhost docs <https://docs.nhost.io>`_ and the `Nhost blog <https://nhost.io/blog>`_.
