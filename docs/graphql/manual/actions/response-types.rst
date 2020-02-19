Action Response Types
=====================


.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


WORK IN PROGRESS

You can return different types of responses when an action is executed.

Basic Response
--------------

WORK IN PROGRESS

.. code-block:: graphql


   mutation place_order($order_input: place_order_input!) {
     place_order(input: $order_input) {
       response {
         order_id
       }
     }
   }

Complex response with relationships
-----------------------------------

WORK IN PROGRESS

.. code-block:: graphql

   mutation place_order($order_input: place_order_input!) {
     place_order(input: $order_input) {
       response {
         order {
           id
           payment_url
           total_amount
           discount
         }
       }
     }
   }

You can fetch relationships of the ``order`` like you would when you query the ``order`` table. Thus with actions you can write the minimum needed code that is needed to validate the mutation and still not lose out on the powerful query fields that graphql-engine generates.

Async response
--------------

WORK IN PROGRESS

.. code-block:: graphql

   mutation place_order($order_input: place_order_input!) {
     place_order(input: $order_input) {
       action_id
     }
   }

Where ``action_id`` is a unique id generated for every async action that has been performed.

