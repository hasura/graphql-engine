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

Step 2: Create a Postgres DB on AWS
-----------------------------------

