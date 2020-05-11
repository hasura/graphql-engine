.. meta::
   :description: Securing Hasura actions
   :keywords: hasura, docs, actions, secure

.. _securing_actions:

Securing actions
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

You will want to make sure that an action can only get called by your own Hasura instance.
You can do so by adding a header to the action that is automatically sent with each request to the webhook.

Step 1: Add an action secret
----------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     Head to the ``Actions -> [action-name]`` tab in the console and scroll down to ``Headers``.
     You 

     .. thumbnail:: /img/graphql/manual/actions/action-secret.png
        :alt: Console action secret
        :width: 75%

     Make sure you tick the checkbox to ``Forward client headers to webhook``. 
     
     Then hit ``Create``.

  .. tab:: CLI

     Go to ``metadata/actions.yaml`` in the Hasura project directory.

     Update the definition of the ``insertAuthor`` action as:

     .. code-block:: yaml
       :emphasize-lines: 4

         - actions
           - name: insertAuthor
             definition:
               kind: synchronous
               handler: '{{ACTIONS_BASE_URL}}/insertAuthor'

     Save the changes and run ``hasura metadata apply`` to set the
     permissions.


This secret is only known by Hasura and is passed to your endpoint with every call, 
thus making sure only Hasura can successfully authenticate with the action handler.

Step 2: Configure production instance
-------------------------------------

In your Hasura production instance, add the action secret as an environment variable.

Step 3: Verify secret in action handler
---------------------------------------

First, load the ``ACTION_SECRET`` as an environment variable in your action handler by adding it to your ``.env`` file 
(this file might be a different one depending on your framework).

In your :ref:`action handler <action_handlers>`, write some code to check that the action secret passed as a header equals to the one you stored as an environment variable.
The following is an example of a simple authorization middleware with Express:

.. code-block:: javascript

    // activate authorization for all routes
    app.use(authorizationMiddleware);

    // autorize action call
    function authorizationMiddleware(req, res, next){
        if (correctSecretProvided(req)) next();
        else res.sendStatus(403);
    }

    // check if the secret sent in the header equals to the secret stored as an env variable
    function correctSecretProvided(req) {
        const requiredSecret = process.env.ACTION_SECRET;
        const providedSecret = req.headers['ACTION_SECRET'];
        return requiredSecret == providedSecret;
    }
