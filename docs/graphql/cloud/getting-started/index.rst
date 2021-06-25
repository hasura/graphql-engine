.. meta::
   :description: Hasura Cloud getting started
   :keywords: hasura, docs, cloud, signup

.. _cloud_getting_started:

Getting Started with Hasura Cloud
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide talks about setting up Hasura Cloud with a new or existing database.

Step 1: Create an account
-------------------------

Navigate to `cloud.hasura.io 
<https://cloud.hasura.io/signup/?pg=docs&plcmt=body&cta=navigate-to-cloud-hasura-io&tech=default>`__, and create a new Hasura Cloud account.

.. _cloud_connect_db:

Step 2: Create project
----------------------

Hasura Cloud creates an initial project. Click ``Launch Console`` to open the Hasura console in your browser.

.. thumbnail:: /img/graphql/cloud/getting-started/create-project.png
   :alt: Connect new or existing database
   :width: 556px

Step 3: Connect new/existing database
-------------------------------------

On the Hasura console, navigate to ``Data -> Manage -> Connect Database``:

.. thumbnail:: /img/graphql/cloud/getting-started/connect-db-console.png
   :alt: Connect database
   :width: 1000px

- To use an existing database, choose ``Connect existing database``.

- To try out with a new Postgres database, choose ``Create Heroku Database``.

.. thumbnail:: /img/graphql/cloud/getting-started/db-setup.png
   :alt: DB setup
   :width: 556px

Step 3.1: Enter database connection URL (for existing database)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you chose ``Connect existing database``, enter your database connection URL.

Click ``Connect Database``.

.. thumbnail:: /img/graphql/cloud/getting-started/connect-db.png
   :alt: Enter URL for existing database
   :width: 556px

If your database is hosted via any of the following managed cloud database services,
check out their respective detailed guides to get the database connection URL and
any other steps required to ensure connectivity from Hasura Cloud:

- :ref:`Aiven Postgres <cloud_db_aiven>`
- :ref:`AWS RDS Aurora Postgres <cloud_db_aws_rds_aurora>`
- :ref:`AWS RDS Postgres <cloud_db_aws_rds_postgres>`
- :ref:`Azure Postgres <cloud_db_azure>`
- :ref:`Digital Ocean Postgres <cloud_db_digital_ocean>`
- :ref:`GCP Postgres <cloud_db_gcp>`
- :ref:`TimescaleDB Cloud <cloud_db_timescale_cloud>`
- :ref:`YugabyteDB <cloud_db_yugabyte>`

Step 4: Try out Hasura
----------------------

Create a table
^^^^^^^^^^^^^^

On the Hasura console, navigate to ``Data -> Create table`` and create a sample table called ``profiles`` with
the following columns:

.. code-block:: sql

  profiles (
    id SERIAL PRIMARY KEY, -- serial -> auto-incrementing integer
    name TEXT
  )

.. thumbnail:: /img/graphql/core/getting-started/create-profile-table.png
   :alt: Create a table

Now, insert some sample data into the table using the ``Insert Row`` tab of the ``profiles`` table.

Try out a query
^^^^^^^^^^^^^^^

Head to the ``GraphiQL`` tab in the console and try running the following query:

.. code-block:: graphql

    query {
      profiles {
        id
        name
      }
    }

You'll see that you get all the inserted data!

.. thumbnail:: /img/graphql/core/getting-started/profile-query.png
   :alt: Try out a query


Check out monitoring
^^^^^^^^^^^^^^^^^^^^

You can navigate to the ``Monitoring`` tab in the console to check out the extra features for reliability and security that Hasura Cloud has
set up for you.

.. thumbnail:: /img/graphql/cloud/metrics/monitoring-tab-overview.png
  :alt: Hasura Console: Monitoring tab

Next steps
----------

Learn course
^^^^^^^^^^^^

For a full hands-on tour of Hasura, check out our `30-Minute Hasura Basics Course <https://hasura.io/learn/graphql/hasura/introduction/>`__.

Database operations
^^^^^^^^^^^^^^^^^^^

- :ref:`Database modelling <schema>`: Learn how to model your database schema, as well as how to extend it.
- :ref:`Querying data <queries>`: Use GraphQL queries to query data from your GraphQL API.
- :ref:`Inserting data <mutations>`: Use GraphQL mutations to insert data into your GraphQL API.

Business logic
^^^^^^^^^^^^^^

There are several options for the implementation of business logic, depending on your use case.

- :ref:`Actions <actions>`: Actions can be used if you'd like to extend your GraphQL schema by integrating with a REST endpoint.
- :ref:`Remote schemas <remote_schemas>`: If you have an existing GraphQL server or if you're comfortable with implementing one, you can use remote schemas.
- :ref:`Event triggers <event_triggers>`: To trigger a serverless function based on a database event, use event triggers.
- :ref:`Scheduled triggers <scheduled_triggers>`: Scheduled triggers are used to execute custom business logic at specific points in time.

Manage Hasura Cloud project
^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can click the gear icon in the Hasura Cloud dashboard to manage your Hasura Cloud project (e.g. add :ref:`collaborators <manage_project_collaborators>`,
:ref:`env vars <manage_project_env_vars>` or :ref:`custom domains <manage_project_domains>`).

.. thumbnail:: /img/graphql/cloud/getting-started/project-manage.png
  :alt: Project actions
  :width: 860px

.. toctree::
   :maxdepth: 1
   :titlesonly:
   :hidden:

   Cloud databases guides <cloud-databases/index>
   Postgres permissions <postgres-permissions>
