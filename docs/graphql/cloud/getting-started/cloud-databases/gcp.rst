.. meta::
   :description: Using Hasura with a GCP Postgres database
   :keywords: hasura, docs, existing database, guide, gcp

.. _cloud_db_gcp:

Using Hasura Cloud with a GCP Postgres database
===============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This guide explains how to connect a new or existing GCP Postgres database to a Hasura Cloud project.

Step 0: Sign up or log in to Hasura Cloud
-----------------------------------------

Navigate to `Hasura Cloud <https://cloud.hasura.io/signup/?pg=docs&plcmt=body&cta=navigate-to-hasura-cloud&tech=default>`__ and sign up or log in.

.. _create_hasura_project_gcp:

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

.. _create_pg_db_gcp:

Step 2: Create a Postgres DB on GCP (skip if you have an existing DB)
---------------------------------------------------------------------

Log into the `GCP console <https://console.cloud.google.com/>`__.

On the left-side navigation, scroll down to ``Storage`` and click on ``SQL``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/gcp/navigate-to-sql.png
   :alt: Navigate to SQL in GCP
   :width: 250px

On the top, click on ``Create instance``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/gcp/create-instance.png
   :alt: Create database instance in GCP
   :width: 1000px

Select Postgres:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/gcp/select-postgres.png
   :alt: Select Postgres database instance in GCP
   :width: 1000px

Select an instance ID, as well as a default user password. If required, choose a specific region and zone. 

.. thumbnail:: /img/graphql/cloud/cloud-dbs/gcp/configure-instance.png
   :alt: Configure database instance in GCP
   :width: 500px

Then click ``Create``.

Step 3: Allow connections to your DB from Hasura Cloud
------------------------------------------------------

On the dashboard of your GCP database instance, on the left sidebar, click on ``Connections``. Then scroll down to the checkbox ``Public IP``, and click ``+ Add network``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/gcp/connections.png
   :alt: Navigate to connections in GCP
   :width: 600px

You can choose an optional name (e.g. "Hasura").

Copy the IP address from the copy icon in the ``Hasura Cloud IP`` field on the project's details view on Hasura Cloud.

.. thumbnail:: /img/graphql/cloud/projects/hasura-cloud-ip.png
   :alt: Hasura Cloud IP field
   :width: 1000px

Enter the Hasura IP address that you copied:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/gcp/add-network.png
   :alt: Add a new network in GCP
   :width: 600px

Then click ``Save``.

.. note::

   If you're using a database user other than the default one, make sure to give it the right :ref:`Postgres permissions <cloud_postgres_permissions>`.

.. _construct_db_url_gcp:

Step 4: Construct the database connection URL
---------------------------------------------

The structure of the database connection URL looks as follows:

.. code-block:: bash

    postgresql://<user-name>:<password>@<public-ip>:<postgres-port>/<db>

- ``user-name``: If you have a separate database user, the user name will be their name. If you didn't specify a user, the default user name is ``postgres``.
- ``password``: If you have a separate database user, use their password. Otherwise, use the password that you chose when creating the database.
- ``public-ip``: The public IP can be optained by clicking on ``Overview`` on the left-side navigation and then scrolling down to ``Connect to this instance``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/gcp/public-ip.png
   :alt: Find the public IP for a GCP Postgres database
   :width: 700px
   
- ``postgres-port``: The default port for Postgres is ``5432`` if not specified otherwise.
- ``db``: The DB is ``postgres`` by default unless otherwise specified.

Step 5: Finish connecting the database
--------------------------------------

Back on Hasura Console, enter the database URL that we retrieved in :ref:`step 4 <construct_db_url_gcp>`:

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
