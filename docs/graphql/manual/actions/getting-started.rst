Getting Started with Actions
============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

An action is a GraphQL mutation. You have to define the GraphQL type of the
arguments that the mutation accepts and the GraphQL type of the its response.

To create an action, you have to:

1. Define the mutation
2. Define the required types
3. Create a handler

Let's start with a basic mutation that accepts a list of numbers and returns
their sum. We'll call this mutation ``addNumbers``.

Console
-------

Go to the ``Actions`` tab on the console and click on ``Create``. This will
take you to a page like this:

.. thumbnail:: ../../../img/graphql/manual/actions/action-create-page.png
   :alt: Open the Hasura console

Define how your action mutation looks like
******************************************

Define the action as follows in the ``Action Definition`` editor.

.. code-block:: graphql


   type Mutation {
     addNumbers (numbers: [Int]): AddResult
   }

The above mutation means:

* This action will be available in your GraphQL schema as a mutation called ``addNumbers``
* It accepts an argument called ``numbers`` which is a list of Integers.
* It returns an output type called ``AddResult``.

Let us define what ``AddResult`` looks like.

Define the required types
*************************

In the above action, we called the returning object type to be ``AddResult``.
Define it in the ``New types definition`` as:

.. code-block:: graphql


   type AddResult {
     sum: Int
   }


This is a simple object type that has a field ``sum`` of type ``Int``.

Creating a handler
******************

A handler is an HTTP webhook where you can perform the custom logic for the
action. In this case, it is the addition of the numbers. NodeJS/Express code
for this handler would look something like:

.. code-block:: js


    const handler = (req, resp) => {
      // You can access ther arguments input at req.body.input
      const { numbers } = req.body.input;

      // perform your custom business logic
      // return an error or response
      try {
        return resp.json({
          sum: numbers.reduce((s, n) => s + n, 0)
        });
      } catch(e) {
        console.error(e)
        return resp.status(500).json({
          message: 'unexpected'
        })
      }
    };



You can deploy this code somewhere and enter the endpoint in the ``Handler``
field. We also have this handler ready at ``https://hasura-actions-starter-kit.glitch.me/addNumbers``.
You can use this to get started quickly.

Finally, hit ``Create``.

Trying it out
*************

Go to ``GraphiQL`` and try out the new action.

.. graphiql::
  :view_only:
  :query:
    mutation MyMutation {
      addNumbers(numbers: [1, 2, 3, 4]) {
        sum
      }
    }
  :response:
    {
      "data": {
        "addNumbers": {
          "sum": 10
        }
      }
    }

And that's it. You have created your first action!


CLI
---

Project setup
*************

Firstly install the latest version of Hasura CLI. You can either get started
with an existing project or create a new project.

**For a new project**

.. code-block:: bash

    hasura init

This will create a new project. You can set up your GraphQL Engine endpoint
(and admin secret if it exists) in the ``config.yaml``.

Run ``hasura metadata export`` so that you get server's metadata into the
``metadata/`` directory.

**For existing projects**

Actions are supported only in the v2 config of the CLI. Check the ``config.yaml``
of your Hasura project for the ``version`` key.

If you are in ``version: 1``, actions commands are not supported. Upgrade to
version 2 by running:

.. code-block:: bash

    hasura scripts update-config-v2

Run ``hasura metadata export`` so that you get server's metadata into the
``metadata/`` directory.

Creating an action
******************

To create an action, run

.. code-block:: bash

    hasura actions create addNumbers

This will open up an editor with ``metadata/actions.graphql``. You can enter
the action's mutation definition and the required types in this file. For your
``addNumbers`` mutation, replace the content of this file with the following
and save:

.. code-block:: graphql


   type Mutation {
     addNumbers (numbers: [Int]): AddResult
   }

   type AddResult {
     sum: Int
   }

The above definition means:

* This action will be available in your GraphQL schema as a mutation called ``addNumbers``
* It accepts an argument called ``numbers`` which is a list of integers.
* It returns an output type called ``AddResult``.
* ``AddResult`` is a simple object type with a field called ``sum`` of type integer.

Creating the handler
********************

A handler is an HTTP webhook where you can perform the custom logic for the
action. In this case, it is the addition of the numbers. NodeJS/Express code
for this handler would look something like:

.. code-block:: js


    const handler = (req, resp) => {
      // You can access ther arguments input at req.body.input
      const { numbers } = req.body.input;

      // perform your custom business logic
      // return an error or response
      try {
        return resp.json({
          sum: numbers.reduce((s, n) => s + n, 0)
        });
      } catch(e) {
        console.error(e)
        return resp.status(500).json({
          message: 'unexpected'
        })
      }
    };



You can deploy this code somewhere and get URI. For getting started quickly, we
also have this handler ready at ``https://hasura-actions-starter-kit.glitch.me/addNumbers``.

Go to ``metadata/actions.yaml``. You must see a handler like ``http://localhost:3000``
or ``http://host.docker.internal:3000`` under the action named ``addNumbers``.
This is a default value taken from ``config.yaml``.

Update the ``handler`` to the above endpoint.

Run ``hasura metadata apply`` to apply this new change to Hasura.

Trying it out
*************

Run ``hasura console`` to open up the console. Go to ``GraphiQL`` and try out
the new action.

.. graphiql::
  :view_only:
  :query:
    mutation MyMutation {
      addNumbers(numbers: [1, 2, 3, 4]) {
        sum
      }
    }
  :response:
    {
      "data": {
        "addNumbers": {
          "sum": 10
        }
      }
    }

And that's it. You have created your first action!
