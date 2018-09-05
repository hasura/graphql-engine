Creating an event trigger
=========================

Event triggers can be created using the Hasura console.

Open the Hasura console, head to the ``Events`` tab and click on the ``Create trigger`` button to open up the
interface below to create an event trigger:

.. image:: ../../../img/graphql/manual/event-triggers/create-event-trigger-annotations.png

Retry configuration
-------------------
Retry configuration is available in the "Advanced settings" when you create a trigger.

1. ``num_retries``: This is the number of times a failed invocation is retried. The default value is **0**.
2. ``interval_sec``: The number of seconds after which a failed invocation for an event, is retried. The default value
   is **10**.
