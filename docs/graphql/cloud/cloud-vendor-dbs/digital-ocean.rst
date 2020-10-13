.. meta::
   :description: Using Hasura with a DO Postgres database
   :keywords: hasura, docs, existing database, guide, digital ocean

.. _cloud_existing_db_do:

Using Hasura Cloud with a Digital Ocean Postgres database
=========================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide explains how to connect a Digital Ocean Postgres database to a Hasura Cloud project.

Before you begin
----------------

Navigate to `Hasura Cloud <https://cloud.hasura.io/>`__ and sign up or log in.

.. _create_hasura_project_do:

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

Step 2: Create a Postgres DB on Digital Ocean (optional)
--------------------------------------------------------

*If you already have an existing database on Digital Ocean, you can skip this step.*

Log into `Digital Ocean <https://cloud.digitalocean.com/>`__.

On the top right, click the ``Create`` button. Then click on ``Databases``:

.. thumbnail:: /img/graphql/cloud/existing-db/do/do-create-database.png
   :alt: Create database on Digital Ocean
   :width: 1000px

Scroll down and choose a ``Cluster configuration``, as well as a ``Datacenter`` based on your requirements.

Scroll to the bottom and choose a unique database cluster name. Also, select a project to which the new database will be associated.

.. thumbnail:: /img/graphql/cloud/existing-db/do/do-cluster-name.png
   :alt: Select cluster name for database on Digital Ocean
   :width: 1000px

Then click ``Create a Database Cluster``.

.. _create_user_account_do:

Step 3: Create a new database user account (optional)
-----------------------------------------------------

If several developers will work with this database, a new database user account can be added for each of them.

Navigate to the database cluster's ``Users & Databases`` page. Add a new user and click ``Save``.

.. thumbnail:: /img/graphql/cloud/existing-db/do/do-add-db-user.png
   :alt: Add database user in Digital Ocean
   :width: 1000px

A password has been generated automatically by Digital Ocean.

Step 4: Connect the Hasura Cloud IP
-----------------------------------

Navigate to the database cluster's ``Overview`` page:

.. thumbnail:: /img/graphql/cloud/existing-db/do/do-db-overview.png
   :alt: Navigate to database overview in Digital Ocean
   :width: 1000px

Scroll down to ``Trusted Sources``. If you see the below warning, click on ``Secure this database cluster by restricting access``:

.. thumbnail:: /img/graphql/cloud/existing-db/do/do-trusted-sources.png
   :alt: Select trusted sources for database in Digital Ocean
   :width: 700px

Scroll down to ``Trusted sources`` and click the ``Edit`` button:

.. thumbnail:: /img/graphql/cloud/existing-db/do/do-edit-trusted-sources.png
   :alt: Edit trusted sources for database in Digital Ocean
   :width: 1000px

Enter the Hasura Cloud IP we have optained from :ref:`step 1 <create_hasura_project_do>`.

.. thumbnail:: /img/graphql/cloud/existing-db/do/do-add-hasura-ip.png
   :alt: Add Hasura IP to database in Digital Ocean
   :width: 700px

Then click ``Save``.

.. _get_db_url_do:

Step 5: Get the database connection URL
---------------------------------------

Navigate to the database cluster's ``Settings`` page:

.. thumbnail:: /img/graphql/cloud/existing-db/do/do-db-settings.png
   :alt: Navigate to database settings in Digital Ocean
   :width: 1000px

Scroll down to ``Connection details``. Select ``Private network`` on the left and ``Connection string`` on the right.

.. thumbnail:: /img/graphql/cloud/existing-db/do/do-connection-string.png
   :alt: Get the database connection string in Digital Ocean
   :width: 600px

Then click the ``Copy`` button for the next step.

Custom database user
^^^^^^^^^^^^^^^^^^^^

If you have added a new database user in :ref:`step 3 <create_user_account_do>`, replace ``doadmin`` with the user name you created.
Also, replace the password with the one that was created with the new user.

.. note::

  When using a custom database user, you need to make sure to give the user appropriate :ref:`Postgres permissions <cloud_postgres_permissions>`.

Step 6: Finish creating the Hasura Cloud project
------------------------------------------------

Back on the Hasura Cloud dashboard, enter the database URL that we configured in :ref:`step 5 <get_db_url_do>`:

.. thumbnail:: /img/graphql/cloud/existing-db/finish-create-project.png
   :alt: Finish creating the Hasura Cloud project
   :width: 500px

Then click ``Create project``.

Step 7: Launch Hasura console
-----------------------------

After the project is initialized successfully, click on ``Launch console``:

.. thumbnail:: /img/graphql/cloud/existing-db/launch-console.png
   :alt: Launch the Hasura console
   :width: 900px

Voilà. You are ready to start developing.

.. thumbnail:: /img/graphql/cloud/existing-db/hasura-console.png
   :alt: Hasura console
   :width: 900px
