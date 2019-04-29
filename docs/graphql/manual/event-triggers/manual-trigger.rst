Manual triggers
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


If you have enabled operation type ``MANUAL`` for your event trigger, then you can use the Hasura console to invoke triggers on rows manually.

Go to your table and browse rows. Choose the run icon next to any row to see the list of event triggers setup on its table:


.. thumbnail:: ../../../img/graphql/manual/event-triggers/select-manual-trigger.png


Click on the event trigger you want to run and a modal will pop-up with the request and response.


.. thumbnail:: ../../../img/graphql/manual/event-triggers/run-manual-trigger.png
