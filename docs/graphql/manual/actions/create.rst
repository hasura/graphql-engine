.. meta::
   :description: Creating Hasura actions
   :keywords: hasura, docs, actions, create 

.. _create_actions:

Creating actions
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

An action is a GraphQL query/mutation. You have to define the GraphQL type of the
arguments that the query/mutation accepts and the GraphQL type of its response.

To create an action, you have to:

1. Define the query/mutation
2. Define the required types
3. Create a handler

**For example**, let's start with a basic mutation that accepts a list of numbers and returns
their sum. We'll call this mutation ``addNumbers``.

Step 0: Setup
-------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     There is no setup required for defining actions via the console.

  .. tab:: CLI

     .. :ref:`Install <install_hasura_cli>` or :ref:`update to <hasura_update-cli>` the latest version of Hasura CLI.

     Download the latest pre-release CLI version from the `releases page <https://github.com/hasura/graphql-engine/releases>`_

     You can either get started with an existing project or create a new project.

     **For a new project**

     .. code-block:: bash

         hasura init

     This will create a new project. You can set up your GraphQL engine endpoint
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


Step 1: Define your mutation and associated types
-------------------------------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     Go to the ``Actions`` tab on the console and click on ``Create``. This will
     take you to a page like this:

     .. thumbnail:: ../../../img/graphql/manual/actions/action-create-page.png
        :alt: Console action create

     Define the action as follows in the ``Action Definition`` editor.

     .. code-block:: graphql

        type Mutation {
          addNumbers (numbers: [Int]): AddResult
        }

     In the above action, we called the returning object type to be ``AddResult``.
     Define it in the ``New types definition`` as:

     .. code-block:: graphql

        type AddResult {
          sum: Int
        }

  .. tab:: CLI

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

Step 2: Create the action handler
---------------------------------

A handler is an HTTP webhook where you can perform the custom logic for the
action. In this case, it is the addition of the numbers. NodeJS/Express code
for this handler would look something like:

.. code-block:: js

    const handler = (req, resp) => {
      // You can access their arguments input at req.body.input
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

Set the handler
***************

Now, set the handler for the action:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     Set the value of the ``handler`` field to the above endpoint.

  .. tab:: CLI

     Go to ``metadata/actions.yaml``. You must see a handler like ``http://localhost:3000``
     or ``http://host.docker.internal:3000`` under the action named ``addNumbers``.
     This is a default value taken from ``config.yaml``.

     Update the ``handler`` to the above endpoint.

.. admonition:: URL templating

  To manage handler endpoints across environments it is possible to template
  the endpoints using ENV variables.

  e.g. ``https://my-handler-endpoint/addNumbers`` can be templated to ``{{ACTION_BASE_ENDPOINT}}/addNumbers``
  where ``ACTION_BASE_ENDPOINT`` is an ENV variable whose value is set to ``https://my-handler-endpoint``

Step 3: Finish action creation
------------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     Hit ``Create``.

  .. tab:: CLI

     Run ``hasura metadata apply``.


Step 4: Try it out
------------------

In the Hasura console, head to the ``GraphiQL`` tab and try out the new action.

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
