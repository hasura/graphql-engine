.. meta::
   :description: Invoke event triggers manually on the Hasura console or over the API
   :keywords: hasura, docs, event trigger, console, invoke

.. _invoke_trigger_manually:

Invoke event trigger manually
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Via console

      You can select the ``Via console`` trigger operation while :ref:`creating an event trigger <create_trigger>`
      to allow invoking the event trigger on rows manually using the Hasura console *(available after version v1.0.0-beta.1)*.

      In the ``Data -> [table-name] -> Browse Rows`` tab, clicking the ``invoke trigger`` button next to any row lets
      you invoke "manual event triggers" configured on the table with that row as payload *(the button will be shown
      only if you have any triggers configured)*:

      .. thumbnail:: /img/graphql/manual/event-triggers/select-manual-trigger.png
         :alt: Invoke event trigger on console

      Click on the event trigger you want to run and a modal will pop up with the request and response.

      .. thumbnail:: /img/graphql/manual/event-triggers/run-manual-trigger.png
         :alt: Request and response of event trigger

   .. tab:: Via API

      When creating an event trigger over the :ref:`create_event_trigger metadata API<create_event_trigger>`, you can set the argument ``enable_manual`` to true.

      Then you can use the :ref:`invoke_event_trigger metadata API<invoke_event_trigger>` to invoke triggers manually:

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type" : "invoke_event_trigger",
            "args" : {
               "name": "send_email",
               "payload": {}
            }
         }
