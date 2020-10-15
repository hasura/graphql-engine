.. meta::
   :description: Using Hasura with an AWS Postgres database
   :keywords: hasura, docs, existing database, guide, aws

.. _cloud_existing_db_aws:

Using Hasura Cloud with an AWS Postgres database
================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This guide explains how to connect an AWS Postgres database to a Hasura Cloud project.

.. note::

   Managed database services only work for the default database user. 
   Support for other database users will be added in the future.

Before you begin
----------------

Navigate to `Hasura Cloud <https://cloud.hasura.io/>`__ and sign up or log in.

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

Step 2: Create a Postgres DB on AWS (optional)
----------------------------------------------

*If you already have an existing database on AWS, you can skip this step.*

Log into the `AWS console <https://console.aws.amazon.com//>`__.

On the top left, click on ``Services`` and type "RDS" into the search field. Then click on ``RDS``:

.. thumbnail:: /img/graphql/cloud/existing-db/aws/aws-search-for-rds.png
   :alt: Navigate to RDS in AWS
   :width: 1000px

Click on the ``Create database`` button:

.. thumbnail:: /img/graphql/cloud/existing-db/aws/aws-create-database.png
   :alt: Create database in AWS
   :width: 1000px

In ``Engine options``, select ``Postgres`` as ``Engine type``:

.. thumbnail:: /img/graphql/cloud/existing-db/aws/aws-rds-select-postgres.png
   :alt: Select Postgres for RDS instance on AWS
   :width: 600px

Scroll down to ``Settings``: 

.. thumbnail:: /img/graphql/cloud/existing-db/aws/aws-rds-settings.png
   :alt: Settings for RDS instance on AWS
   :width: 600px

Now you can choose a ``DB instance identifier`` as a name for your database. The ``Master username`` is ``postgres`` by default. 
You can change that if you have to. As for the password, you can click the checkbox for AWS to auto-generate one for you, or you can type in a password of your choice.

Scroll down and customize other database attributes such as ``DB instance size`` and ``Storage``, based on your requirements.

In the ``Connectivity`` section, set ``Public access`` to ``Yes`` and choose or add a new security group:

.. thumbnail:: /img/graphql/cloud/existing-db/aws/aws-rds-connectivity.png
   :alt: Connectivity for RDS instance on AWS
   :width: 600px

When you're done, at the bottom, click the ``Create database`` button:

.. thumbnail:: /img/graphql/cloud/existing-db/aws/aws-rds-click-create.png
   :alt: Create RDS instance on AWS
   :width: 700px

Step 3: Connect the Hasura Cloud IP
-----------------------------------

.. _configure_db_url_aws:

Step 4: Configure the database connection URL
---------------------------------------------

Step 5: Finish creating the Hasura Cloud project
------------------------------------------------

Back on the Hasura Cloud dashboard, enter the database URL that we configured in :ref:`step 4 <configure_db_url_aws>`:

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
