.. meta::
   :description: Get started with Hasura using Heroku
   :keywords: hasura, docs, start, heroku

.. _quickstart:

Quickstart
==========

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Heroku
^^^^^^

Get started :ref:`using Heroku <heroku_simple>` in under 60 seconds with no setup required to
host the Hasura GraphQL engine and Postgres on Heroku's free tier.

Docker
^^^^^^

Run a **local development** setup that sets up both the Hasura GraphQL
engine and Postgres :ref:`using Docker <docker_simple>`.

Existing database
^^^^^^^^^^^^^^^^^

If you'd like to set up Hasura with an existing database, follow these guides:

- :ref:`Heroku <heroku_existing_db>`: Run on Heroku and connect to an existing Heroku Postgres database.
- :ref:`Docker <deployment_docker>`: Run on Kubernetes and connect to an existing Postgres
  database.

First steps
^^^^^^^^^^^

.. _first_graphql_query:

Make your first GraphQL query
"""""""""""""""""""""""""""""

Let's create a sample table and query data from it using the Hasura console, a UI tool meant for doing exactly this:

Create a table
--------------

Head to the Hasura console, navigate to ``Data -> Create table`` and create a sample table called ``profile`` with
the following columns:

.. code-block:: sql

  profile (
    id INT PRIMARY KEY,
    name TEXT
  )

.. thumbnail:: ../../../img/graphql/manual/getting-started/create-profile-table.png
   :alt: Create a table

Now, insert some sample data into the table using the ``Insert Row`` tab of the ``profile`` table.

Try out a query
---------------

Head to the ``GraphiQL`` tab in the console and try running the following query:

.. code-block:: graphql

    query {
      profile {
        id
        name
      }
    }

You'll see that you get all the inserted data!

.. thumbnail:: ../../../img/graphql/manual/getting-started/profile-query.png
   :alt: Try out a query

Next steps
----------

Read more about:

- :ref:`Building your schema <schema>`
- :ref:`Queries <queries>`

.. _first_event_trigger:

Set up your first event trigger
"""""""""""""""""""""""""""""""

You can use Hasura to setup event triggers that call configured webhooks whenever specific database events occur.

Let's create a sample event trigger with https://httpbin.org as our simple webhook.

Create a table
--------------
Head to the Hasura console, navigate to ``Data -> Create table`` and create a sample table called ``profile`` with
the following columns:

.. code-block:: sql

  profile (
    id INT PRIMARY KEY,
    name TEXT
  )

.. thumbnail:: ../../../img/graphql/manual/getting-started/create-profile-table.png
   :alt: Create a table

Setup an event trigger
----------------------
In the Hasura console, navigate to ``Events -> Create trigger`` and:

1. Enter trigger name as ``echo``.
2. Select table ``profile`` from the table dropdown.
3. Select operations: ``insert``, ``update`` and ``delete``.
4. Enter webhook URL as: ``https://httpbin.org/post``.

.. thumbnail:: ../../../img/graphql/manual/getting-started/create-event-trigger.png
   :alt: Set up an event trigger

This sets up our webhook ``https://httpbin.org/post`` to receive database changes on an insert, update and delete on
``profile`` table.


Watch the trigger in action
---------------------------

1. Insert some sample data into the ``profile`` table using the ``Insert Row`` tab.
2. Now navigate to the ``Events`` tab and click on the ``echo`` trigger in the left sidebar.
3. Expand the details of an event to see the response from the webhook.

.. thumbnail:: ../../../img/graphql/manual/getting-started/trigger-events.png
   :alt: Trigger in action

Next steps
----------

Read more about :ref:`event triggers <event_triggers>`.


.. toctree::
   :maxdepth: 2
   :titlesonly:
   :hidden:

   Heroku quickstart <quickstart/heroku-simple>
   Docker quickstart <quickstart/docker-simple>
