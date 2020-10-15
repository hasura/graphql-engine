.. meta::
   :description: Using Hasura with an Azure Postgres database
   :keywords: hasura, docs, existing database, guide, azure

.. _cloud_existing_db_azure:

Using Hasura Cloud with an Azure Postgres database
==================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This guide explains how to connect an Azure Postgres database to a Hasura Cloud project.

.. note::

   Managed database services only work for the default database user. 
   Support for other database users will be added in the future.

Before you begin
----------------

Navigate to `Hasura Cloud <https://cloud.hasura.io/>`__ and sign up or log in.

.. _create_hasura_project_azure:

Step 1: Create a Hasura Cloud project
-------------------------------------

On the Hasura Cloud dashboard, create a new project:

.. thumbnail:: /img/graphql/cloud/existing-db/create-hasura-cloud-project.png
   :alt: Create Hasura Cloud project
   :width: 1000px

You will get prompted for a Postgres Database URL. We will create this in the next step and then come back here.

.. thumbnail:: /img/graphql/cloud/existing-db/database-setup.png
   :alt: Hasura Cloud database setup
   :width: 500px

Also, copy the Hasura Cloud IP for later.

Step 2: Create a Postgres DB on Azure (optional)
------------------------------------------------

*If you already have an existing database on Digital Ocean, you can skip this step.*

Log into `Azure <https://portal.azure.com>`__.

On the Azure portal, type "postgres" in the search window and choose ``Azure Database for PostgreSQL servers``:

Back on the Hasura Cloud dashboard, enter the database URL that we configured in :ref:`step 4 <get_db_url_do>`:

.. thumbnail:: /img/graphql/cloud/existing-db/azure/azure-select-postgres-db.png
   :alt: Select Postgres database on Azure
   :width: 1000px

Click the ``+ Add`` button to create a new Postgres database:

.. thumbnail:: /img/graphql/cloud/existing-db/azure/azure-add-postgres-db.png
   :alt: Add Postgres database on Azure
   :width: 1000px

Choose the plan that fits your requirements. For this tutorial, we'll choose ``Single server``:

.. thumbnail:: /img/graphql/cloud/existing-db/azure/azure-pg-single-server.png
   :alt: Select single server on Azure
   :width: 600px

Configure your database with all required fields:

.. thumbnail:: /img/graphql/cloud/existing-db/azure/azure-configure-db.png
   :alt: Configure database on Azure
   :width: 600px

Then click ``Review + create``.

Step 3: Connect the Hasura Cloud IP
-----------------------------------

Step 4: Get the database connection URL
---------------------------------------

The structure of the database connection URL looks as follows:

.. code-block:: bash

    postgresql://<user-name>:<password>@<public-ip>:<postgres-port>/<db>

Step 5: Finish creating the Hasura Cloud project
------------------------------------------------

Back on the Hasura Cloud dashboard, enter the database URL that we configured in :ref:`step 4 <get_db_url_do>`:

.. thumbnail:: /img/graphql/cloud/existing-db/finish-create-project.png
   :alt: Finish creating the Hasura Cloud project
   :width: 500px

Then click ``Create project``.

Step 6: Launch Hasura console
-----------------------------

After the project is initialized successfully, click on ``Launch console``:

.. thumbnail:: /img/graphql/cloud/existing-db/launch-console.png
   :alt: Launch the Hasura console
   :width: 900px

Voilà. You are ready to start developing.

.. thumbnail:: /img/graphql/cloud/existing-db/hasura-console.png
   :alt: Hasura console
   :width: 900px
