.. meta::
   :description: Using Hasura with a DO Postgres database
   :keywords: hasura, docs, existing database, guide, digital ocean

.. _cloud_db_digital_ocean:

Using Hasura Cloud with a Digital Ocean Postgres database
=========================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide explains how to connect a new or existing Digital Ocean Postgres database to a Hasura Cloud project.

Step 0: Sign up or log in to Hasura Cloud
-----------------------------------------

Navigate to `Hasura Cloud <https://cloud.hasura.io/signup/?pg=docs&plcmt=body&cta=navigate-to-hasura-cloud&tech=default>`__ and sign up or log in.

.. _create_hasura_project_do:

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

Step 2: Create a Postgres DB on Digital Ocean (skip if you have an existing DB)
-------------------------------------------------------------------------------

Log into `Digital Ocean <https://cloud.digitalocean.com/>`__.

On the top right, click the ``Create`` button. Then click on ``Databases``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/do/create-database.png
   :alt: Create database on Digital Ocean
   :width: 1000px

Scroll down and choose a ``Cluster configuration``, as well as a ``Datacenter`` based on your requirements.

Scroll to the bottom and choose a unique database cluster name. Also, select a project the new database will be associated with.

.. thumbnail:: /img/graphql/cloud/cloud-dbs/do/cluster-name.png
   :alt: Select cluster name for database on Digital Ocean
   :width: 1000px

Then click ``Create a Database Cluster``.

Step 3: Allow connections to your DB from Hasura Cloud
------------------------------------------------------

Navigate to the database cluster's ``Overview`` page:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/do/db-settings.png
   :alt: Navigate to database settings in Digital Ocean
   :width: 1000px

Scroll down to ``Trusted sources`` and click the ``Edit`` button:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/do/edit-trusted-sources.png
   :alt: Edit trusted sources for database in Digital Ocean
   :width: 1000px

Copy the IP address from the copy icon in the ``Hasura Cloud IP`` field on the project's details view on Hasura Cloud.

.. thumbnail:: /img/graphql/cloud/projects/hasura-cloud-ip.png
   :alt: Hasura Cloud IP field
   :width: 1000px

Enter the Hasura IP address that you copied:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/do/add-hasura-ip.png
   :alt: Add Hasura IP to database in Digital Ocean
   :width: 700px

Then click ``Save``.

.. note::

   If you're using a database user other than the default one, make sure to give it the right :ref:`Postgres permissions <cloud_postgres_permissions>`.

.. _get_db_url_do:

Step 4: Get the database connection URL
---------------------------------------

The structure of the database connection URL looks as follows:

.. code-block:: bash

    postgresql://<user-name>:<password>@<public-ip>:<postgres-port>/<db>

To get it, navigate to the database cluster's ``Overview`` page:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/do/db-overview.png
   :alt: Navigate to database overview in Digital Ocean
   :width: 1000px

Scroll down to ``Connection details``. Select ``Public network`` on the left and ``Connection string`` on the right.

.. thumbnail:: /img/graphql/cloud/cloud-dbs/do/connection-string.png
   :alt: Get the database connection string in Digital Ocean
   :width: 600px

Then click the ``Copy`` button for the next step.

Step 5: Finish connecting the database
--------------------------------------

Back on Hasura Console, enter the database URL that we retrieved in :ref:`step 4 <get_db_url_do>`:

.. thumbnail:: /img/graphql/cloud/getting-started/connect-db.png
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
