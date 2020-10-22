.. meta::
   :description: Using Hasura with a Timescale Postgres database
   :keywords: hasura, docs, existing database, guide, timescale

.. _cloud_db_timescale_cloud:

Using Hasura Cloud with a Timescale Postgres database
=====================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This guide explains how to connect a new or existing Timescale Postgres database to a Hasura Cloud project.

Step 0: Sign up or log in to Hasura Cloud
-----------------------------------------

Navigate to `Hasura Cloud <https://cloud.hasura.io/>`__ and sign up or log in.

.. _create_hasura_project_timescale:

Step 1: Create a Hasura Cloud project
-------------------------------------

On the Hasura Cloud dashboard, create a new project:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/create-hasura-cloud-project.png
   :alt: Create Hasura Cloud project
   :width: 1000px

You will get prompted for a Postgres Database URL. We will create this in the next step and then come back here.

.. thumbnail:: /img/graphql/cloud/cloud-dbs/database-setup.png
   :alt: Hasura Cloud database setup
   :width: 500px

Also, copy the Hasura Cloud IP for later.

.. _create_pg_db_timescale:

Step 2: Create a Postgres DB on Timescale (skip if you have an existing DB)
---------------------------------------------------------------------------

Log into the `Timescale Cloud portal <https://portal.timescale.cloud/login>`__.

On the Timescale dashboard, under ``Services``, click on ``+ Create new service``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/timescale/create-new-service.png
   :alt: Create a new service on Timescale
   :width: 1000px

Select the Postgres option:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/timescale/select-postgres.png
   :alt: Select Postgres on Timescale
   :width: 700px

Scroll down and select the ``Cloud Provider``, ``Region`` and ``Service Plan`` based on your requirements.

In the end, enter a ``Name`` for the service:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/timescale/create-service.png
   :alt: Create a service on Timescale
   :width: 1000px

Then click ``Create service``.

Step 3: Allow connections to your DB from Hasura Cloud
------------------------------------------------------

On the ``Services`` dashboard, click on your DB:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/timescale/select-db.png
   :alt: Select DB on Timescale
   :width: 1000px

Scroll down to ``Allowed IP Addresses`` and click on ``Change``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/timescale/change-allowed-ip-addresses.png
   :alt: Change allowed IP addresses on Timescale
   :width: 1000px

Add the Hasura IP address that you copied in :ref:`step 1 <create_hasura_project_timescale>`, click on the ``+`` and then click on ``Save changes``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/timescale/add-hasura-ip.png
   :alt: Add the Hasura IP on Timescale
   :width: 500px

.. _get_db_url_timescale:

Step 4: Get the database connection URL
---------------------------------------

The structure of the database connection URL looks as follows:

.. code-block:: bash

    postgresql://<user-name>:<password>@<public-ip>:<postgres-port>/<db>

To get it, navigate to the ``Overview`` tab of your database dashboard and copy the ``Service URI``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/timescale/copy-service-uri.png
   :alt: Copy the service URI on Timescale
   :width: 1000px

Step 5: Finish creating the Hasura Cloud project
------------------------------------------------

Back on the Hasura Cloud dashboard, enter the database URL that we retrieved in :ref:`step 4 <get_db_url_timescale>`:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/finish-create-project.png
   :alt: Finish creating the Hasura Cloud project
   :width: 500px

Then click ``Create project``.

Step 6: Launch Hasura console
-----------------------------

After the project is initialized successfully, click on ``Launch console``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/launch-console.png
   :alt: Launch the Hasura console
   :width: 900px

Voilà. You are ready to start developing.

.. thumbnail:: /img/graphql/cloud/cloud-dbs/hasura-console.png
   :alt: Hasura console
   :width: 900px

Next steps
----------

You can check out our `30-Minute Hasura Basics Course <https://hasura.io/learn/graphql/hasura/introduction/>`__
and other `GraphQL & Hasura Courses <https://hasura.io/learn/>`__ for a more detailed introduction to Hasura.

You can also click the gear icon to manage your Hasura Cloud project. (e.g. add :ref:`collaborators <manage_project_collaborators>`,
:ref:`env vars <manage_project_env_vars>` or :ref:`custom domains <manage_project_domains>`) and :ref:`add an admin secret <secure_project>`
to make sure that your GraphQL endpoint and the Hasura console are not publicly accessible.

.. thumbnail:: /img/graphql/cloud/getting-started/project-manage.png
  :alt: Project actions
  :width: 860px

