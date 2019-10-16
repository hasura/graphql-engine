Async Actions
=============


.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

WORK IN PROGRESS

Sometimes you may not want to wait for an action to complete (say if the business logic takes a long time). In such cases you can create an **asynchronous** action, which returns an ``action_id`` immediately to the client before contacting the webhook.

If you mark an action as **asynchronous**, graphql-engine also generates a query and a subscription field for the action so that you can query/subscribe to its status. In the above example, let's say ``place_order`` is an asnychronous action, your client code looks something like this:

.. code-block:: graphql

   mutation place_order($order_input: place_order_input!) {
     place_order(input: $order_input) {
       action_id
     }
   }

.. code-block:: graphql

   subscription order_status($action_id: uuid!) {
     place_order(action_id: $action_id) {
       order {
         id
         payment_url
         total_amount
         discount
       }
     }
   }
