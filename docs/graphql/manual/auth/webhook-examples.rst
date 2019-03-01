Auth webhook samples
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

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
- ``/firebase/webhook`` (`View source <https://github.com/hasura/graphql-engine/blob/master/community/boilerplates/auth-webhooks/nodejs-express/firebase/firebaseHandler.js>`__) 

.. note::

    If you are using ``firebase`` you will have to set the associated environment variables.
