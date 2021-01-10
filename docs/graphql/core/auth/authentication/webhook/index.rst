.. meta::
   :description: Use authenticaton with webhooks in Hasura
   :keywords: hasura, docs, authentication, auth, webhook

.. _auth_webhooks:

Authentication using webhooks
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

You can configure the GraphQL engine to use a webhook to authenticate all incoming requests to the Hasura GraphQL engine server. 

.. thumbnail:: /img/graphql/core/auth/webhook-auth.png
   :alt: Authentication using webhooks

.. admonition:: Prerequisite
   
   It is mandatory to first :ref:`secure your GraphQL endpoint <securing_graphql_endpoint>` for the webhook mode to take effect.

In webhook mode, on a secured endpoint:

- The configured webhook is  **called** when the ``X-Hasura-Admin-Secret`` header is not found in the request.
- The configured webhook is **ignored** when the ``X-Hasura-Admin-Secret`` header is found in the request and
  admin access is granted.


Auth webhook samples
--------------------

We have put together a `GitHub Node.js repo <https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/auth-webhooks/nodejs-express>`__ that has some sample auth
webhooks configured.

You can deploy these samples using `glitch <https://glitch.com/>`__:

.. image:: https://raw.githubusercontent.com/hasura/sample-auth-webhook/master/assets/deploy-glitch.png
   :width: 200px
   :alt: deploy_auth_webhook_with_glitch
   :class: no-shadow
   :target: http://glitch.com/edit/#!/import/github/hasura/sample-auth-webhook

Once deployed, you can use any of the following endpoints as your auth webhook in the GraphQL engine:

- ``/simple/webhook``  (`View source <https://github.com/hasura/graphql-engine/blob/master/community/boilerplates/auth-webhooks/nodejs-express/server.js>`__)
- ``/firebase/webhook`` (`View source <https://github.com/hasura/graphql-engine/blob/master/community/boilerplates/auth-webhooks/nodejs-firebase/firebase/firebaseHandler.js>`__)

.. note::

   If you are using ``Firebase``, you will have to set the associated environment variables.

Integrating webhook authentication
----------------------------------

.. toctree::
  :maxdepth: 1

  Webhook spec <spec>
  Configure webhook mode <configure>
  Advanced use cases <advanced>
  Integration guides <guides/index>
