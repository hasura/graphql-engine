Actions
=======

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Actions are custom mutations that can be added to graphql-engine to handle various use cases such as validation, complex business logic etc.

When the permissions system isn't enough to specify the required constraints, you would typically add such mutation through a remote schema, however actions can handle these use cases better because of the following reasons:

1. Actions let you return graphql-engine's types without writing any resolvers for them.
2. They also provide the developers with a powerful asynchronous model for mutations which should enable building applications with the CQRS pattern.

Example
-------
Let's say you are building an ecommerce application where you need to provide a mutation for placing an 'order', ``place_order``, you will need to first define the input types for this mutation:

.. code-block:: graphql

   enum payment_method {
     stripe
     paytm
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

You will then define an action called ``place_order`` with ``place_order_input`` as the "input" type, ``place_order_response`` as the "output" type and a http endpoint to be invoked when this action is called by the client. The logic could look something like this:

.. code-block:: python

   def place_order(payload):
       input_args = payload['input']
       session_variables = payload['session_variables']
       # code to validate this mutation and insert into the database
       order_id = validate_and_insert_order(input_args, session_variables)
       return {"order_id": order_id}

Once you have the action setup, you'll have to define the permissions for the role for which you want to allow this action. For all such roles, this action will be exposed as a mutation. The client would then execute this mutation as follows:

.. code-block:: graphql

   mutation place_order($order_input: place_order_input!) {
     place_order(input: $order_input) {
       action_id
       response {
         order_id
       }
     }
   }

Where ``action_id`` is a unique id generated for every action that has been performed. The response from the webhook can be accessed through the ``response`` field.

Relationships
-------------

As you may have noticed this isn't very different from what you would have done with remote schemas. This is where relationships on actions come in, letting you hook into the powerful schema that graphql-engine generates. Since the webhook returns an ``order_id``, you can link it to the ``order`` table through an object relationship on ``place_order_response`` called ``order``. The ``order`` information can be requested as follows:

.. code-block:: graphql

   mutation place_order($order_input: place_order_input!) {
     place_order(input: $order_input) {
       action_id
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

Asynchronous actions
--------------------

Sometimes you may not want to wait for an action to complete (say if the business logic takes a long time). In such cases you can create an "asynchronous" action, which returns an ``action_id`` immediately to the client before contacting the webhook.

If you mark an action as "asynchronous", graphql-engine also generates a query and a subscription field for the action so that you can query/subscribe to its status. In the above example, let's say ``place_order`` is an asnychronous action, your client code looks something like this:

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
