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

