.. meta::
   :description: Set up a first event trigger with Hasura
   :keywords: hasura, docs, start, event trigger

.. _first_event_trigger:

Setting up your first event trigger
===================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can use Hasura to setup event triggers that call configured webhooks whenever specific database events occur.

Let's create a sample event trigger with https://httpbin.org as our simple webhook.

Create a table
--------------

Let's add the following table:

.. code-block:: sql

  profile (
    id INT PRIMARY KEY,
    name TEXT
  )

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Via console

      Head to the Hasura console, navigate to ``Data -> Create table`` and create a sample table called ``profile`` with the following columns:

      .. thumbnail:: /img/graphql/manual/getting-started/create-profile-table.png
         :alt: Create a table

   .. tab:: Via CLI

      :ref:`Create a migration manually <manual_migrations>` with the following statement:

      .. code-block:: sql

         CREATE TABLE profile(id serial NOT NULL, name text NOT NULL);

      Then apply the migration by running:

      .. code-block:: bash

         hasura migrate apply

      To track the table and expose it over the GraphQL API, add it to the ``tables.yaml`` file in the ``metadata`` directory as follows:

      .. code-block:: yaml
         :emphasize-lines: 1-3

         - table:
               schema: public
               name: profile

      Then apply the metadata by running:

      .. code-block:: bash

         hasura metadata apply

   .. tab:: Via API

      Create a table by by using the :ref:`run_sql API <run_sql>`:

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type": "run_sql",
            "args": {
               "sql": "CREATE TABLE profile(id serial NOT NULL, name text NOT NULL);"
            }
         }

      To track the table and expose it over the GraphQL API, use the :ref:`track_table API <track_table>`:

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
         "type": "track_table",
         "args": {
            "schema": "public",
            "name": "profile"
         }
         }

Setup an event trigger
----------------------

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Via console

      In the Hasura console, navigate to ``Events -> Create trigger`` and:

      1. Enter trigger name as ``echo``.
      2. Select table ``profile`` from the table dropdown.
      3. Select operations: ``insert``, ``update`` and ``delete``.
      4. Enter webhook URL as: ``https://httpbin.org/post``.

      .. thumbnail:: /img/graphql/manual/getting-started/create-event-trigger.png
         :alt: Set up an event trigger

   .. tab:: Via CLI

      Add an event trigger in the ``tables.yaml`` file in the ``metadata`` directory as follows:

      .. code-block:: yaml
         :emphasize-lines: 4-20

        - table:
            schema: public
            name: profile
         event_triggers:
         - name: echo
            definition:
               enable_manual: false
               insert:
               columns: '*'
               delete:
               columns: '*'
               update:
               columns:
               - id
               - name
            retry_conf:
               num_retries: 0
               interval_sec: 10
               timeout_sec: 60
            webhook: https://httpbin.org/post

      Then apply the metadata by running:

      .. code-block:: bash

         hasura metadata apply

   .. tab:: Via API

      Add an event trigger by using the :ref:`create_event_trigger<create_event_trigger>` API.

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type": "create_event_trigger",
            "args": {
               "name": "echo",
               "table": {
                     "name": "profile",
                     "schema": "public"
               },
               "webhook": "https://httpbin.org/post",
               "insert": {
                     "columns": "*"
               },
               "update": {
                     "columns": "*"
               },
               "delete": {
                     "columns": "*"
               }
            }
         }

This sets up our webhook ``https://httpbin.org/post`` to receive database changes on an insert, update and delete on
``profile`` table.


Watch the trigger in action
---------------------------

1. Insert some sample data into the ``profile`` table using the ``Insert Row`` tab.
2. Now navigate to the ``Events`` tab and click on the ``echo`` trigger in the left sidebar.
3. Expand the details of an event to see the response from the webhook.

.. thumbnail:: /img/graphql/manual/getting-started/trigger-events.png
   :alt: Trigger in action

Next steps
----------

Read more about:

- :ref:`Event triggers <event_triggers>`
