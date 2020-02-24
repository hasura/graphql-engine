Async Actions
=============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Sometimes you may not want to wait for an action to complete (say if the
business logic takes a long time). In such cases you can create an
**asynchronous** action, which returns an ``action_id`` immediately to the
client before contacting the webhook.

If you mark an action as **asynchronous**, graphql-engine also generates a
query and a subscription field for the action so that you can query/subscribe
to its status. In the above example, let's say ``place_order`` is an
asynchronous action, your client code looks something like this:

.. code-block:: graphql

   mutation place_order($order_input: place_order_input!) {
     place_order(input: $order_input) 
   }

This will return you a response like:

.. code-block:: graphql

   {
     "data": {
       "place_order": "23b1c256-7aff-4b95-95bd-68220d9f93f2"
     }
   }

The returned ``uuid`` is the action id of the async action. To get the response
from the action, you can ``query`` or ``subscribe`` to the action using this
action id.

.. code-block:: graphql

    query MyQuery {
      place_order (id: "23b1c256-7aff-4b95-95bd-68220d9f93f2") {
        output
        errors
      }
    }

