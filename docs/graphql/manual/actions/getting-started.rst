Getting Started with Actions
============================


.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

WORK IN PROGRESS

Let's say you are building an ecommerce application where you need to provide a mutation for placing an 'order', ``place_order``.

Example
-------

WORK IN PROGRESS

First, you will need to first define the input types for this mutation in the console:

.. code-block:: graphql

   enum payment_method {
     stripe
     paypal
   }

   input type place_order_input {
     selected_payment_mode payment_method!
     items [order_item_input!]!
     address_id uuid!
     coupon_code String
   }

   input order_item_input {
     skuId uuid!
     quantity Int!
   }

   type place_order_response {
     order_id uuid!
   }

You will then define an action called ``place_order`` with ``place_order_input`` as the **input** type, ``place_order_response`` as the **output** type.

Once you have the action setup, you'll have to define the permissions for the role for which you want to allow this action. For all such roles, this action will be exposed as a mutation. The client can then execute this mutation as follows:

.. code-block:: graphql

   mutation place_order($order_input: place_order_input!) {
     place_order(input: $order_input) {
       response {
         order_id
       }
     }
   }


But how is this action executed? An action can be linked to different types of handlers (see: :doc:`Action handlers <action-handlers>`) . In this example, let's use a HTTP handler which will be invoked when this action is called by the client. The logic of this handler could look something like this:

.. code-block:: python

   def place_order(payload):
       input_args = payload['input']
       session_variables = payload['session_variables']
       order_id = validate_and_insert_order(input_args, session_variables) # some business logic code
       return {"order_id": order_id}

And that's it. You have created your first action!
