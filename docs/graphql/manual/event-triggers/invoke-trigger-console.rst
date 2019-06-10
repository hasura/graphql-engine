Invoke event trigger via console
================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can select the ``Via console`` trigger operation while :doc:`creating an event trigger <./create-trigger>`
to allow invoking the event trigger on rows manually using the Hasura console. *(available after version v1.0.0-beta.1)*

In the ``Data -> [table-name] -> Browse Rows`` tab, clicking the invoke trigger button next to any row lets
you invoke manual event triggers configured on the table with that row as payload *(the button will be shown
only if you have any triggers configured)*:

.. thumbnail:: ../../../img/graphql/manual/event-triggers/select-manual-trigger.png

Click on the event trigger you want to run and a modal will pop-up with the request and response.

.. thumbnail:: ../../../img/graphql/manual/event-triggers/run-manual-trigger.png

.. note::

  You can also use the :ref:`invoke_event_trigger` metadata API to invoke manual triggers