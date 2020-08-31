.. meta::
   :description: Custom webhook integration with NodeJS for Hasura
   :keywords: hasura, docs, guide, authentication, auth, webhook, integration, nodejs

.. _guides_custom_webhook:

Custom webhook server with NodeJS
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide will show how to write a custom webhook server for Hasura using Node & Express JS.

Before you begin
----------------

Before starting with this guide, you should have the following ready:

- An empty Node & ExpressJS app

- An account on Heroku

Step 1: Set up auth webhook
---------------------------

Inside your main file (you can call it ``app.js`` or ``server.js``), add the following code:

.. code-block:: javascript

  // init project
  const express = require('express');
  const app = express();
  const requestClient = require('request');
  const port = process.env.PORT || 3000;

  // fetch user info in a mock function
  function fetchUserInfo (token, cb) {
    const role = token['x-hasura-role']
    const user_id = token['x-hasura-user-id']

    cb({role: role, user_id: user_id});
  }
  app.get('/', (req, res) => {
    res.send('Webhooks are running');
  });

  app.get('/webhook', (request, response) => {

    // Extract token from request
    const token = request.get('Authorization')

    // Fetch user_id that is associated with this token
    fetchUserInfo(token, (result) => {

      // Return appropriate response to Hasura
      const hasuraVariables = {
        'X-Hasura-Role': result.role, 
        'X-Hasura-User-Id': result.user_id    
      };
      response.json(hasuraVariables);
    });
  });

  // listen for requests 
  const listener = app.listen(port, function () {
    console.log('Your app is listening on port ' + port);
  });

Step 2: Deploy the webhook
--------------------------

Now let's deploy the webhook. You can deploy it to any cloud provider. In this example, we'll use Heroku.

Step 2.1: Commit your webhook to Git
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Inside your project directory, run:

.. code-block:: bash

  git init && git add . && git commit -m "init auth webhook"

Step 2.2: Set up a Heroku app
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Inside your project directory, run:

.. code-block:: bash

  heroku apps:create
  git push heroku master

You will get back a URL with your doployed webhook. If you open it in your browser, you will see the following: ``Webhooks are running.``

Step 3: Set up webhook mode in Hasura
-------------------------------------

There are two options to configure Hasura to run in webhook mode:

1. Running the GraphQL engine with the ``--auth-hook`` flag 
2. Add the ``HASURA_GRAPHQL_AUTH_HOOK`` environment variable 

The value is the webhook endpoint. In this tutorial, the endpoint looks like this: ``https://my-auth-webhook.herokuapp.com/webhook`` where ``my-auth-webhook`` should be replaced by your own app name.

.. note::

  See :ref:`GraphQL engine server options <server_flag_reference>` for more information on flags and environment variables.

Step 4: Test your auth webhook
------------------------------

From now on, whenever a request comes in to Hasura, the auth webhook will be called. 

Make an API call to your Hasura endpoint and see how the webhook returns the ``role`` and the ``user_id``.

Next steps
----------

The next step would be not to use a mock function in ``fetchUserInfo``, but to take a token and then make an async call to the session cache or database to fetch
data that is needed for Hasura's access control rules.
