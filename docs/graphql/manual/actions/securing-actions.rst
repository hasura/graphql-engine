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

You will want to make sure that an action can only get called by your own Hasura instance and not by third parties.
You can do so by adding a header to the action that is automatically sent with each request to the webhook.

Add an action secret
--------------------

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
       :emphasize-lines: 7-9

         - actions
           - name: actionName
             definition:
                kind: synchronous
                handler: http://localhost:3000
                forward_client_headers: true
                headers:
                  - value: ACTION_SECRET
                    name: mysecret

     Save the changes and run ``hasura metadata apply`` to set the
     headers.


This secret is only known by Hasura and is passed to your endpoint with every call, 
thus making sure only Hasura can successfully authenticate with the action handler.

.. note::

    The name for the action secret is not defined by Hasura and can be chosen freely.

Configure production instance
-----------------------------

In your Hasura production instance, add the ``ACTION_SECRET`` as an environment variable.

Verify secret in action handler
-------------------------------

First, load the ``ACTION_SECRET`` as an environment variable in your action handler by adding it to your ``.env`` file 
(this file might be a different one depending on your framework).

In your :ref:`action handler <action_handlers>`, you need to write some code to check that the action secret passed as a header equals to the one you stored as an environment variable.
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
