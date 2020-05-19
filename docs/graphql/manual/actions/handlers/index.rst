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
        "x-hasura-user-id": "<session user id>",
        "x-hasura-role": "<session user role>"
      }
    }


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
      "message": "<mandatory error message>",
      "code": "<optional error code>"
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

Securing your action handler
----------------------------

In order to secure your action handler, refer to the following page:

.. toctree::
  :maxdepth: 1
  :titlesonly:

  Securing action handlers <securing-handler>
