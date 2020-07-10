.. meta::
   :description: Action handlers for Hasura actions
   :keywords: hasura, docs, actions, handlers

.. _action_handlers:

Action handlers
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Actions need to be backed by custom business logic. This business logic can be
defined in a handler which is an HTTP webhook.


HTTP handler
------------

When the action is executed i.e. when the query or the mutation is called, Hasura makes a ``POST`` request to the
handler with the action arguments and the session variables.

The request payload is of the format:

.. code-block:: json

    {
      "action": {
        "name": "<action-name>"
      },
      "input": {
        "arg1": "<value>",
        "arg2": "<value>"
      },
      "session_variables": {
        "x-hasura-user-id": "<session-user-id>",
        "x-hasura-role": "<session-user-role>"
      }
    }

.. note::

    All ``session_variables`` in the request payload have lowercase keys.



Returning a success response
----------------------------

To return a success response, you must send back a response payload of action's
response type. The HTTP status code must be ``2xx`` for a successful response.

Returning an error response
---------------------------

To return an error response, you must send back an error object.
An error object looks like:

.. code-block:: json

    {
      "message": "<mandatory-error-message>",
      "code": "<optional-error-code>"
    }

The HTTP status code must be ``4xx`` for an error response.


Example
-------

For example, consider the following mutation.

.. code-block:: graphql

    extend type Mutation {
      UserLogin (username: String!, email: String!): UserInfo
    }

    type UserInfo {
      accessToken: String!
      userId: Int!
    }

Let's say, the following mutation is executed:

.. code-block:: graphql

    mutation {
      UserLogin (username: "jake", password: "secretpassword") {
        accessToken
        userId
      }
    }


Hasura will call the handler with the following payload:

.. code-block:: json

    {
      "action": {
        "name": "UserLogin"
      },  
      "input": {
        "username": "jake",
        "password": "secretpassword"
      },
      "session_variables": {
        "x-hasura-user-id": "423",
        "x-hasura-role": "user"
      }
    }

To return a success response, you must send the response of the action's output
type (in this case, ``UserInfo``) with a status code ``2xx``. So a sample
response would be:

.. code-block:: json

    {
      "accessToken": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVC",
      "userId": 4829
    }

To throw an error, you must a response payload of the following type while
setting the status code as ``4xx``.

.. code-block:: json

   {
     "message": "invalid credentials"
   }

.. _securing_action_handlers:

Securing your action handler
----------------------------

You might want to make sure that an action handler can only get called by your
Hasura instance and not by third parties.

Adding an action secret
^^^^^^^^^^^^^^^^^^^^^^^

One possible way of securing an action handler is by adding a header to the action
that is automatically sent with each request to the webhook, and then adding a check
against that in your action handler.

.. contents::
  :backlinks: none
  :depth: 1
  :local:

.. note::

  Adding an action secret is a simple way of securing an action
  handler against unauthorized access and will suffice in most use cases.
  However, if you have more profound security requirements, you might want to choose advanced
  security solutions tailored to your needs.


Step 1: Configure your Hasura instance
**************************************

In your Hasura server, add the action secret as an
environment variable, say ``ACTION_SECRET_ENV``.

Step 2: Add a header to your action
***********************************

For your action, add a header that will act as an action secret.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     Head to the ``Actions -> [action-name]`` tab in the console and scroll down to ``Headers``.
     You can now configure an action secret by adding a header:

     .. thumbnail:: /img/graphql/manual/actions/action-secret-header.png
        :alt: Console action secret
        :width: 75%

     Then hit ``Save``.

  .. tab:: CLI

     Go to ``metadata/actions.yaml`` in the Hasura project directory.

     Update the definition of your action by adding the action secret as a header:

     .. code-block:: yaml
       :emphasize-lines: 7-9

           - actions
             - name: actionName
               definition:
                  kind: synchronous
                  handler: http://localhost:3000
                forward_client_headers: true
                headers:
                  - name: ACTION_SECRET
                    value_from_env: ACTION_SECRET_ENV

     Save the changes and run ``hasura metadata apply`` to set the
     headers.


This secret is only known by Hasura and is passed to your endpoint with every call,
thus making sure only Hasura can successfully authenticate with the action handler.

.. note::

    The name for the action secret is not defined by Hasura and can be chosen freely.

Step 3: Verify the secret in your action handler
************************************************

First, load the action secret as an environment variable in your action handler by adding it to your ``.env`` file
(this file might be a different one depending on your framework).

Second, you need to write some code in your action handler to check that the action secret
passed as a header equals to the one you stored as an environment variable.

The following is an example of a simple authorization middleware with Express:

.. code-block:: javascript

    // use authorization for all routes
    app.use(authorizationMiddleware);

    // authorize action call
    function authorizationMiddleware(req, res, next){
        if (correctSecretProvided(req)) next();
        else res.sendStatus(403);
    }

    // check if the secret sent in the header equals to the secret stored as an env variable
    function correctSecretProvided(req) {
        const requiredSecret = process.env.ACTION_SECRET_ENV;
        const providedSecret = req.headers['ACTION_SECRET'];
        return requiredSecret == providedSecret;
    }

