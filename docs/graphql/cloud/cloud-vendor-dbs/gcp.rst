.. meta::
   :description: Using Hasura with a GCP Postgres database
   :keywords: hasura, docs, existing database, guide, gcp

.. _cloud_existing_db_gcp:

Using Hasura Cloud with a GCP Postgres database
===============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This guide explains how to connect a GCP Postgres database to a Hasura Cloud project.

.. note::

   Managed database services only work for the default database user. 
   Support for other database users will be added in the future.

Before you begin
----------------

Navigate to `Hasura Cloud <https://cloud.hasura.io/>`__ and sign up or log in.

.. _create_hasura_project_gcp:

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

.. _create_pg_db_gcp:

Step 2: Create a Postgres DB on GCP (optional)
----------------------------------------------

*If you already have an existing database on GCP, you can skip this step.*

Log into the `GCP console <https://console.cloud.google.com/>`__.

On the left-side navigation, scroll down to ``Storage`` and click on ``SQL``:

.. thumbnail:: /img/graphql/cloud/existing-db/gcp/gcp-navigate-to-sql.png
   :alt: Navigate to SQL in GCP
   :width: 250px

On the top, click on ``Create instance``:

.. thumbnail:: /img/graphql/cloud/existing-db/gcp/gcp-create-instance.png
   :alt: Create database instance in GCP
   :width: 1000px

Select Postgres:

.. thumbnail:: /img/graphql/cloud/existing-db/gcp/gcp-select-postgres.png
   :alt: Select Postgres database instance in GCP
   :width: 1000px

Select an instance ID, as well as a default user password. If required, choose a specific region and zone. 

.. thumbnail:: /img/graphql/cloud/existing-db/gcp/gcp-configure-instance.png
   :alt: Configure database instance in GCP
   :width: 500px

Then click ``Create``.

Step 3: Connect the Hasura Cloud IP
-----------------------------------

On the dashboard of your GCP database instance, on the left sidebar, click on ``Connections``. Then scroll down to the checkbox ``Public IP``, and click ``+ Add network``:

.. thumbnail:: /img/graphql/cloud/existing-db/gcp/gcp-connections.png
   :alt: Navigate to connections in GCP
   :width: 600px

You can choose an optional name (e.g. "Hasura"). Then enter the Hasura Cloud IP we have optained from :ref:`step 1 <create_hasura_project_gcp>`.

.. thumbnail:: /img/graphql/cloud/existing-db/gcp/gcp-add-network.png
   :alt: Add a new network in GCP
   :width: 600px

Then click ``Save``.

.. _configure_db_url_gcp:

Step 4: Configure the database connection URL
---------------------------------------------

The structure of the database connection URL looks as follows:

.. code-block:: bash

    postgresql://<user-name>:<password>@<public-ip>:<postgres-port>/<db>

If you have added a new user account in :ref:`step 3 <create_user_account_gcp>`, the user name refers to the one you created there.

If you didn't specify a user name, it is ``postgres`` by default and can be optained by navigating to ``Databases``:

.. thumbnail:: /img/graphql/cloud/existing-db/gcp/gcp-db-user-name.png
   :alt: Find the user name for a GCP Postgres database
   :width: 700px

If you have added a new user account in :ref:`step 3 <create_user_account_gcp>`, the password refers to the one you created there.

Otherwise, the password is the one we set when we created the database instance in :ref:`step 2 <create_pg_db_gcp>`.

The public IP can be optained by clicking on ``Overview`` on the left-side navigation and then scrolling down to ``Connect to this instance``:

.. thumbnail:: /img/graphql/cloud/existing-db/gcp/gcp-public-ip.png
   :alt: Find the public IP for a GCP Postgres database
   :width: 700px

The Postgres port is ``5432`` by default, but it can be customized.

Step 5: Finish creating the Hasura Cloud project
------------------------------------------------

Back on the Hasura Cloud dashboard, enter the database URL that we configured in :ref:`step 5 <configure_db_url_gcp>`:

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
