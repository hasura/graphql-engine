.. meta::
   :description: Async actions
   :keywords: hasura, docs, actions, async actions

.. _async_actions:

Async actions
=============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Sometimes you may not want to wait for an action to complete before sending a
response back to the client (say if the business logic takes a long time). In
such cases you can create an **asynchronous** action, which returns an
``action_id`` immediately to the client before contacting the handler.

If you mark an action as **asynchronous**, Hasura also generates a
``query`` and a ``subscription`` field for the action so that you can
query/subscribe to its status.

For example, let's say ``place_order`` is an asynchronous action

.. code-block:: graphql

   mutation placeOrderRequest($order_input: place_order_input!) {
     place_order(input: $order_input) 
   }

Executing this mutation will return a response like:

.. code-block:: json

   {
     "data": {
       "place_order": "23b1c256-7aff-4b95-95bd-68220d9f93f2"
     }
   }

The returned ``uuid`` is the ``action id`` of the async action. To get the actual
response of the action, you can ``query`` or ``subscribe`` to the action
using this ``action id``.

.. code-block:: graphql

    subscription getPlaceOrderResponse {
      place_order (id: "23b1c256-7aff-4b95-95bd-68220d9f93f2") {
        output
        errors
      }
    }

