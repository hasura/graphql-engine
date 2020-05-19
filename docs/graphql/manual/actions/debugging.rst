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

While you're developing actions for your application, to debug faster you may
want to see the exact details of the webhook call for the action
as well.

To do so, start the server in :ref:`debugging mode <dev-mode>`.
In the case of errors, the GraphQL response will contain debugging information
of the webhook calls in the ``extensions.internal`` field.

**For example**:

.. graphiql::
  :view_only:
  :query:
    mutation {
        create_user(email: "foo@bar.com", name: "Foo"){
          id
          user {
            name
            email
          }
        }
      }
  :response:
    {
      "errors": [
        {
          "message": "expecting object or array of objects for action webhook response",
          "extensions": {
            "code": "parse-failed",
            "path": "$",
            "internal": {
              "error": "expecting object or array of objects for action webhook response",
              "response": {
                "status": 200,
                "headers": [
                  {
                    "value": "application/json",
                    "name": "Content-Type"
                  }
                ],
                "body": "[incorrect response]"
              },
              "request": {
                "url": "http://127.0.0.1:5593/invalid-response",
                "headers": [],
                "body": {
                  "action": {
                    "name": "create_user"
                  },
                  "session_variables": {
                    "x-hasura-role": "admin"
                  },
                  "input": {
                    "email": "foo@boo.com",
                    "name": "Foo"
                  }
                }
              }
            }
          }
        }
      ]
    }