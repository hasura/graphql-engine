.. meta::
   :description: Using Hasura with an Azure Postgres database
   :keywords: hasura, docs, existing database, guide, azure

.. _cloud_db_azure:

Using Hasura Cloud with an Azure Postgres database
==================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This guide explains how to connect a new or existing Azure Postgres database to a Hasura Cloud project.

Step 0: Sign up or log in to Hasura Cloud
-----------------------------------------

Navigate to `Hasura Cloud <https://cloud.hasura.io/signup/?pg=docs&plcmt=body&cta=navigate-to-hasura-cloud&tech=default>`__ and sign up or log in.

.. _create_hasura_project_azure:

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

Step 2: Create a Postgres DB on Azure (skip if you have an existing DB)
-----------------------------------------------------------------------

Log into the `Azure portal <https://portal.azure.com>`__.

On the Azure portal, type "postgres" in the search window and choose ``Azure Database for PostgreSQL servers``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/azure/select-postgres-db.png
   :alt: Select Postgres database on Azure
   :width: 1000px

Click the ``+ Add`` button to create a new Postgres database:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/azure/add-postgres-db.png
   :alt: Add Postgres database on Azure
   :width: 1000px

Choose the plan that fits your requirements. For this tutorial, we'll choose ``Single server``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/azure/pg-single-server.png
   :alt: Select single server on Azure
   :width: 600px

Configure your database with all required fields:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/azure/configure-db.png
   :alt: Configure database on Azure
   :width: 600px

Then click ``Review + create``.

.. note::

   If you're using a database user other than the default one, make sure to give it the right :ref:`Postgres permissions <cloud_postgres_permissions>`.

Step 3: Allow connections to your DB from Hasura Cloud
------------------------------------------------------

On the database dashboard, click on ``Connection security`` under ``Settings`` on the left navigation bar. 

On ``Allow access to Azure services``, click the ``Yes`` button. Then add a Firewall rule for Hasura. 

Copy the IP address from the copy icon in the ``Hasura Cloud IP`` field on the project's details view on Hasura Cloud.

.. thumbnail:: /img/graphql/cloud/projects/hasura-cloud-ip.png
   :alt: Hasura Cloud IP field
   :width: 1000px

Add the Hasura IP address that you copied:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/azure/add-hasura-ip.png
   :alt: Add Hasura IP on Azure
   :width: 1000px

Then click ``Save`` on the top left.

.. _get_db_url_azure:

Step 4: Construct the database connection URL
---------------------------------------------

The structure of the database connection URL looks as follows:

.. code-block:: bash

    postgresql://<user-name>:<password>@<public-ip>:<postgres-port>/<db>

On the database dashboard, click on ``Overview``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/azure/get-database-connection-string.png
   :alt: Construct the database connection string for Azure
   :width: 1000px

- ``user-name``: If you have a separate database user, the user name will be their name. If you didn't specify a user, use the ``Admin username`` (see screenshot above). **Note:** you need to escape the ``@``. Replace it with ``%40``.
- ``password``: If you have a separate database user, use their password. Otherwise, use the password that you chose when creating the database.
- ``public-ip``: On the screenshot above, the ``Server name`` is the public IP.
- ``postgres-port``: The default port for Postgres is ``5432``.
- ``db``: The DB is ``postgres`` by default unless otherwise specified.

Step 5: Finish connecting the database
--------------------------------------

Back on Hasura Console, enter the database URL that we retrieved in :ref:`step 4 <get_db_url_azure>`:

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
