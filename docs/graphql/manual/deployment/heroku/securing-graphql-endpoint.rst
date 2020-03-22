.. meta::
   :description: Secure Hasura GraphQL endpoint with Heroku deployment
   :keywords: hasura, docs, deployment, heroku, secure

.. _heroku_secure:

Securing the GraphQL endpoint (Heroku)
======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an admin secret key.


Add the HASURA_GRAPHQL_ADMIN_SECRET env var
-------------------------------------------

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

      Head to the config-vars URL on your Heroku dashboard and set the ``HASURA_GRAPHQL_ADMIN_SECRET`` environment variable.

      .. thumbnail:: ../../../../img/graphql/manual/deployment/secure-heroku.png
         :alt: Add an admin secret

      Setting this environment variable will automatically restart the dyno. Now when you access your console, you'll be
      prompted for the admin secret key.

      .. thumbnail:: ../../../../img/graphql/manual/deployment/access-key-console.png
         :alt: Prompt for the admin secret

   .. tab:: CLI

      If you open the Hasura console from the CLI, use the ``admin-secret`` flag when you open the console:

      .. code-block:: bash

         hasura console --admin-secret=myadminsecretkey

.. note::

  The ``HASURA_GRAPHQL_ADMIN_SECRET`` should never be passed from the client to Hasura GraphQL engine as it would
  give the client full admin rights to your Hasura instance. See :ref:`auth` for information on
  setting up authentication.

