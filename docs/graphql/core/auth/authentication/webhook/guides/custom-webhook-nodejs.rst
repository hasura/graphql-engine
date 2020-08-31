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

Create a Node & ExpressJS app.

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




