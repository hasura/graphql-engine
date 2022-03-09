.. meta::
   :description: Create an event trigger with Hasura
   :keywords: hasura, docs, event trigger, create

.. _create_trigger:

Creating an event trigger
=========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Event triggers can be created using the Hasura console, CLI or metadata APIs.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

      Open the Hasura console, head to the ``Events`` tab and click on the ``Create`` button to open the
      page below:

      .. thumbnail:: /img/graphql/core/event-triggers/create-event-trigger.png
         :alt: Create an event trigger

   .. tab:: CLI

      You can add an event triggers in the ``tables.yaml`` file inside the ``metadata`` directory:

      .. code-block:: yaml
         :emphasize-lines: 4-12

         - table:
            schema: public
            name: author
           event_triggers:
           - name: author_trigger
             definition:
               enable_manual: false
               insert:
                 columns: '*'
               update:
                 columns: '*'
             webhook: https://httpbin.org/post

      Apply the metadata by running:

      .. code-block:: bash

         hasura metadata apply

   .. tab:: API

      Add an event trigger by using the :ref:`metadata_pg_create_event_trigger` metadata API.

      .. code-block:: http

         POST /v1/metadata HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type" : "pg_create_event_trigger",
            "args" : {
               "name": "author_trigger",
               "source": "<db_name>",
               "table": {
                  "name": "author",
                  "schema": "public"
               },
               "webhook": "https://httpbin.org/post",
               "insert": {
                   "columns": "*"
               },
               "update": {
                   "columns": "*"
               }
            }
         }

Parameters
----------

**Trigger Name**

Unique name for event trigger.

**Schema/Table**

The postgres schema and table name on which the event trigger needs to be created.

**Trigger Operations**

The table operation on which the event trigger will be invoked.

**Webhook URL**

The HTTP(s) URL which will be called with the event payload on configured operation. Must be a ``POST`` handler. This URL
can be entered manually or can be picked up from an environment variable (*the environment variable needs to be set
before using it for this configuration*).

.. note::

  If you are running Hasura using Docker, ensure that the Hasura Docker container can reach the webhook.
  See :ref:`this page <docker_networking>` for Docker networking.

Advanced Settings
-----------------

.. contents::
  :backlinks: none
  :depth: 1
  :local:


Listen columns for update
^^^^^^^^^^^^^^^^^^^^^^^^^

Update operations are special because you may want to trigger a webhook only if specific columns have changed in a row.
Choose the columns here which you want the update operation to listen to.

If a column is not selected here, then an update to that column will not trigger the webhook.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

     Expand the ``Advanced Settings`` section on the Hasura console to define advanced settings for an event trigger:

     .. thumbnail:: /img/graphql/core/event-triggers/create-event-trigger-listen-columns.png
        :alt: Listen columns for update event triggers

   .. tab:: CLI

      You can configure advanced settings for event triggers in the ``tables.yaml`` file inside the ``metadata`` directory:

      .. code-block:: yaml
         :emphasize-lines: 10-13

           - table:
              schema: public
              name: author
             event_triggers:
             - name: author_trigger
               definition:
                 enable_manual: false
                 insert:
                   columns: '*'
                 update:
                   columns:
                   - name
                   - addr
               webhook: https://httpbin.org/post

      Apply the metadata by running:

      .. code-block:: bash

         hasura metadata apply

   .. tab:: API

      You can configure advanced settings via the :ref:`metadata_pg_create_event_trigger` metadata API.

      .. code-block:: http
         :emphasize-lines: 18-20

         POST /v1/metadata HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type": "pg_create_event_trigger",
            "args": {
               "name": "author_trigger",
               "source": "<db_name>",
               "table": {
                  "name": "author",
                  "schema": "public"
               },
               "webhook": "https://httpbin.org/post",
               "insert": {
                  "columns": "*"
               },
               "update": {
                  "columns": ["name", "addr"]
               },
               "retry_conf": {
                  "num_retries": 0,
                  "interval_sec": 10,
                  "timeout_sec": 60
               },
               "headers": [
                  {
                     "name": "X-Hasura-From-Val",
                     "value": "static-value"
                  },
                  {
                     "name": "X-Hasura-From-Env",
                     "value_from_env": "EVENT_WEBHOOK_HEADER"
                  }
               ],
               "replace": false
            }
         }

Retry Logic
^^^^^^^^^^^

Retry configuration is available in the "Advanced settings" when you create a trigger.

1. ``num_retries``: Number of times a failed invocation is retried. Default value is **0**.
2. ``interval_sec``: Number of seconds after which a failed invocation is retried. Default value is **10**.
3. ``timeout_sec``:: Number of seconds before which client closes the connection to the webhook. Default value is **60**.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

      Expand the ``Advanced Settings`` section on the Hasura console to define advanced settings for an event trigger:

      .. thumbnail:: /img/graphql/core/event-triggers/create-event-trigger-retry.png
         :alt: Retry settings for event triggers

   .. tab:: CLI

      You can configure advanced settings for event triggers in the ``tables.yaml`` file inside the ``metadata`` directory:

      .. code-block:: yaml
         :emphasize-lines: 12-15

           - table:
              schema: public
              name: author
             event_triggers:
             - name: author_trigger
               definition:
                 enable_manual: false
                 insert:
                   columns: '*'
                 update:
                   columns: ['name']
               retry_conf:
                 num_retries: 0
                 interval_sec: 10
                 timeout_sec: 60
               headers:
               - name: X-Hasura-From-Val
                 value: static-value'
               - name: X-Hasura-From-Env
                 value_from_env: EVENT_WEBHOOK_HEADER
               webhook: https://httpbin.org/post

      Apply the metadata by running:

      .. code-block:: bash

         hasura metadata apply

   .. tab:: API

      You can configure advanced settings via the :ref:`metadata_pg_create_event_trigger` metadata API.

      .. code-block:: http
         :emphasize-lines: 21-25

         POST /v1/metadata HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type": "pg_create_event_trigger",
            "args": {
               "name": "author_trigger",
               "source": "<db_name>",
               "table": {
                  "name": "author",
                  "schema": "public"
               },
               "webhook": "https://httpbin.org/post",
               "insert": {
                  "columns": "*"
               },
               "update": {
                  "columns": ["name"]
               },
               "retry_conf": {
                  "num_retries": 0,
                  "interval_sec": 10,
                  "timeout_sec": 60
               },
               "headers": [
                  {
                     "name": "X-Hasura-From-Val",
                     "value": "static-value"
                  },
                  {
                     "name": "X-Hasura-From-Env",
                     "value_from_env": "EVENT_WEBHOOK_HEADER"
                  }
               ],
               "replace": false
            }
         }


Headers
^^^^^^^

Custom headers can be added to an event trigger. Each webhook request will have these headers added.

Each header has 3 parameters:

1. ``Key``: Name of the header e.g. Authorization or X-My-Header.
2. ``Type``: One of ``static`` or ``from env variable``. ``static`` means the value provided in the ``Value`` field is
   the raw value of the header. ``from env variable`` means the value provided in the ``Value`` field is the name of
   the environment variable in the GraphQL engine which will be resolved before sending the header.
3. ``Value``: The value of the header. Either a static value or the name of an environment variable.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

      Expand the ``Advanced Settings`` section on the Hasura console to define advanced settings for an event trigger:

      .. thumbnail:: /img/graphql/core/event-triggers/create-event-trigger-headers.png
         :alt: Headers for event triggers

   .. tab:: CLI

      You can configure advanced settings for event triggers in the ``tables.yaml`` file inside the ``metadata`` directory:

      .. code-block:: yaml
         :emphasize-lines: 16-20

           - table:
              schema: public
              name: author
             event_triggers:
             - name: author_trigger
               definition:
                 enable_manual: false
                 insert:
                   columns: '*'
                 update:
                   columns: ['name']
               retry_conf:
                 num_retries: 0
                 interval_sec: 10
                 timeout_sec: 60
               headers:
               - name: X-Hasura-From-Val
                 value: static-value'
               - name: X-Hasura-From-Env
                 value_from_env: EVENT_WEBHOOK_HEADER
               webhook: https://httpbin.org/post

      Apply the metadata by running:

      .. code-block:: bash

         hasura metadata apply

   .. tab:: API

      You can configure advanced settings via the :ref:`metadata_pg_create_event_trigger` metadata API.

      .. code-block:: http
         :emphasize-lines: 26-35

         POST /v1/metadata HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type": "pg_create_event_trigger",
            "args": {
               "name": "author_trigger",
               "source": "<db_name>",
               "table": {
                  "name": "author",
                  "schema": "public"
               },
               "webhook": "https://httpbin.org/post",
               "insert": {
                  "columns": "*"
               },
               "update": {
                  "columns": ["name"]
               },
               "retry_conf": {
                  "num_retries": 0,
                  "interval_sec": 10,
                  "timeout_sec": 60
               },
               "headers": [
                  {
                     "name": "X-Hasura-From-Val",
                     "value": "static-value"
                  },
                  {
                     "name": "X-Hasura-From-Env",
                     "value_from_env": "EVENT_WEBHOOK_HEADER"
                  }
               ],
               "replace": false
            }
         }
