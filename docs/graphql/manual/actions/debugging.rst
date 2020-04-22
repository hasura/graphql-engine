.. meta::
   :description: Deriving Hasura actions
   :keywords: hasura, docs, actions, debug, debugging

.. _debugging_actions:

Debugging actions
=================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

While you're developing actions for your application, you may want to debug the webhook
configured for the action. To do so, start the server in :ref:`debugging mode <errors-debugging>`.
In the case of errors, the GraphQL response contains debugging information of webhook
calls in the ``extensions.internal`` field.

For example, let's consider the following mutation:

.. code-block:: graphql

      mutation {
        create_user(email: "foo@bar.com", name: "Foo"){
          id
          user {
            name
            email
            is_admin
          }
        }
      }

In case of errors, the response will look like the following if the debugging mode is enabled.

.. code-block:: json

    {
      "errors": [
        {
          "extensions": {
            "internal": {
              "debug_info": {
                "response": {
                  "status": 200,
                  "body": "some-string",
                  "headers": [
                    {
                      "value": "application/json",
                      "name": "Content-Type"
                    }
                  ]
                },
                "request": {
                  "body": {
                    "session_variables": {
                      "x-hasura-role": "admin"
                    },
                    "input": {
                      "email": "foo@boo.com",
                      "name": "Foo"
                    },
                    "action": {
                      "name": "create_user"
                    }
                  },
                  "url": "http://127.0.0.1:5593/invalid-response",
                  "headers": [

                  ]
                }
              },
              "error": "expecting object or array of objects for action webhook response",
              "type": "unexpected_json"
            },
            "path": "$",
            "code": "parse-failed"
          },
          "message": "expecting object or array of objects for action webhook response"
        }
      ]
    }
