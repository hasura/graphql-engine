.. meta::
  :description: Getting Started with Hasura & BigQuery
  :keywords: hasura, docs, databases, bigquery

.. _database_bigquery_getting_started:

Getting started with BigQuery
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Pre-requisites
--------------

Hasura GraphQL engine requires the following to connect to a BigQuery project:

- `Project Id <https://support.google.com/googleapi/answer/7014113?hl=en>`__
- The `datasets <https://cloud.google.com/bigquery/docs/datasets-intro>`__ that can be exposed over graphql have to be explicitly listed.
- A `Service Account <https://cloud.google.com/iam/docs/service-accounts>`__ to query the project.

Creating a Service Account
--------------------------

- In Google Cloud's console, head to your BigQuery project.

- Go to ``IAM & Admin > Service Accounts > Create Service Account``

  .. thumbnail:: /img/graphql/core/databases/bigquery/1-service-account.png
     :alt: Add source
     :width: 400px


- Give it a name, and under roles, and grant these 3 roles: ``Bigquery Metadata Viewer``, ``BigQuery Data Viewer`` and ``BigQuery Job User``.

  .. thumbnail:: /img/graphql/core/databases/bigquery/2-service-account-details.png
     :alt: Add source
     :width: 600px

- Click on the created service account, ``Keys > ADD KEY > Create New Key > JSON > Create``. This will download a service account file on your computer.

Connecting to a BigQuery project
--------------------------------

- Run Graphql engine with an environment variable set to the contents of the service account. This maybe done as follows if you are using docker:

.. code-block:: bash

      docker run -e BIGQUERY_SA_ACCOUNT=$(cat /path/to/the/service-account.json) <rest-of-the-flags>


- Head to the console, in the ``Connect Existing Database`` page, choose ``Environment Variable`` under ``Connect Via``, and fill in the necessary details:

.. thumbnail:: /img/graphql/core/databases/bigquery/3-connect-bigquery.png
   :alt: Add source
   :width: 600px

You should now be able to track the tables that are part of the specified 
tables and configure relationships between them. As BigQuery lacks 
foreign key constraints, the console cannot suggest relationships, so 
all relationships between BigQuery tables have to be manually configured.

Try out a GraphQL query
-----------------------

Head to the ``GraphiQL`` tab in the console and try running a GraphQL query! Use the explorer sidebar on GraphQL to get help in creating a GraphQL query.

.. thumbnail:: /img/graphql/core/databases/ms-sql-server/6-make-graphql-query.png
   :alt: Make GraphQL query
   :width: 1000px

Keep up to date
---------------

Hasura currently supports queries on BigQuery.

Please watch this space to get the latest docs on how you can try these features out via the console or by manipulating metadata in JSON/YAML directly.

If you'd like to stay informed about the status of BigQuery support, subscribe to our newsletter and join our discord!

- https://hasura.io/newsletter/
- https://discord.com/invite/hasura
