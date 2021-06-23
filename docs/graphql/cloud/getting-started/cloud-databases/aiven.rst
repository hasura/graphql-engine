.. meta::
   :description: Using Hasura with an Aiven Postgres database
   :keywords: hasura, docs, existing database, guide, aiven

.. _cloud_db_aiven:

Using Hasura Cloud with an Aiven Postgres database
==================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This guide explains how to connect a new or existing Aiven Postgres database to a Hasura Cloud project.

Step 0: Sign up or log in to Hasura Cloud
-----------------------------------------

Navigate to `Hasura Cloud <https://cloud.hasura.io/signup/?pg=docs&plcmt=body&cta=navigate-to-hasura-cloud&tech=default>`__ and sign up or log in.

.. _create_hasura_project_aiven:

Step 1: Create a Hasura Cloud project
-------------------------------------

On the Hasura Cloud dashboard, create a new project:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/create-hasura-cloud-project.png
   :alt: Create Hasura Cloud project
   :width: 1000px

After the project is initialized successfully, click on ``Launch console`` to open the Hasura console in your browser.

On the Hasura console, navigate to ``Data -> Manage -> Connect Database -> Connect existing database``:

You will get prompted for a Postgres Database URL. We will create this in the next step and then come back here.

.. thumbnail:: /img/graphql/cloud/cloud-dbs/existing-db-setup.png
   :alt: Hasura Cloud database setup
   :width: 700px

.. _create_pg_db_aiven:

Step 2: Create a Postgres DB on Aiven (skip if you have an existing DB)
-----------------------------------------------------------------------

Log into the `Aiven console <https://console.aiven.io/login>`__.

On the Aiven console, click ``+ Create a new service``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/aiven/create-new-service.png
   :alt: Create a new service on Aiven
   :width: 1000px

Select ``Postgres``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/aiven/select-postgres.png
   :alt: Select Postgres on Aiven
   :width: 1000px

Scroll down and select the ``Cloud Provider``, ``Region`` and ``Service Plan`` based on your requirements.

In the end, enter a ``Name`` for the service:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/aiven/create-service.png
   :alt: Create a service on Aiven
   :width: 1000px

Then click ``Create service``.

.. note::

   If you're using a database user other than the default one, make sure to give it the right :ref:`Postgres permissions <cloud_postgres_permissions>`.

Step 3: Allow connections to your DB from Hasura Cloud
------------------------------------------------------

On the ``Services`` dashboard, click on your DB:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/aiven/select-db.png
   :alt: Select DB on Aiven
   :width: 1000px

Scroll down to ``Allowed IP Addresses`` and click on ``Change``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/aiven/change-allowed-ip-addresses.png
   :alt: Change allowed IP addresses on Aiven
   :width: 1000px

Copy the IP address from the copy icon in the ``Hasura Cloud IP`` field on the project's details view on Hasura Cloud.

.. thumbnail:: /img/graphql/cloud/projects/hasura-cloud-ip.png
   :alt: Hasura Cloud IP field
   :width: 1000px

Add the Hasura IP address that you copied, click on the ``+``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/aiven/add-hasura-ip.png
   :alt: Add the Hasura IP on Aiven
   :width: 1000px

Then click on ``Save changes``.

.. _get_db_url_aiven:

Step 4: Get the database connection URL
---------------------------------------

The structure of the database connection URL looks as follows:

.. code-block:: bash

    postgresql://<user-name>:<password>@<public-ip>:<postgres-port>/<db>

To get it, navigate to the ``Overview`` tab of your database dashboard and copy the ``Service URI``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/aiven/copy-service-uri.png
   :alt: Copy the service URI on Aiven
   :width: 1000px

Step 5: Finish connecting the database
--------------------------------------

Back on Hasura Console, enter the database URL that we retrieved in :ref:`step 4 <get_db_url_aiven>`:

.. thumbnail:: /img/graphql/cloud/getting-started/connect-db.png
   :alt: Database setup
   :width: 600px

Then click ``Connect Database``.

.. note::

   For security reasons, it is recommended to set database URLs as :ref:`env vars <manage_project_env_vars>` and using the env vars
   to connect to the databases in place of the raw database URLs.

Voilà. You are ready to start developing.

.. thumbnail:: /img/graphql/cloud/cloud-dbs/hasura-console.png
   :alt: Hasura console
   :width: 1100px

Next steps
----------

You can check out our `30-Minute Hasura Basics Course <https://hasura.io/learn/graphql/hasura/introduction/>`__
and other `GraphQL & Hasura Courses <https://hasura.io/learn/>`__ for a more detailed introduction to Hasura.

You can also click the gear icon to manage your Hasura Cloud project. (e.g. add :ref:`collaborators <manage_project_collaborators>`,
:ref:`env vars <manage_project_env_vars>` or :ref:`custom domains <manage_project_domains>`).

.. thumbnail:: /img/graphql/cloud/getting-started/project-manage.png
  :alt: Project actions
  :width: 860px

