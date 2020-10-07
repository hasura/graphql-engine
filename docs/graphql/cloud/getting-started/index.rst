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

Step 1: Create an account
-------------------------

Navigate to `cloud.hasura.io 
<https://cloud.hasura.io/login>`__, and create a new Hasura Cloud account.

.. _cloud_connect_db:

Step 2: Connect new/existing database
-------------------------------------

- To use an existing database, choose ``I have an existing Postgres database``.
- To try out with a new database, choose ``Try a free database with Heroku``.

.. thumbnail:: /img/graphql/cloud/getting-started/connect-db.png
   :alt: Connect new or existing database
   :width: 591px

Step 2.1: Enter database URL (for existing database)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you chose ``I have an existing Postgres database`` in :ref:`Step 2 <cloud_connect_db>`, enter
your database connection URL.

.. thumbnail:: /img/graphql/cloud/getting-started/connect-existing-db.png
   :alt: Enter URL for existing database
   :width: 556px

Step 3: Create project
----------------------

Click ``Create Project``.

.. thumbnail:: /img/graphql/cloud/getting-started/create-project-new-db.png
   :alt: Create project for new database
   :width: 539px
   :group: create
   :class: inline-block

.. thumbnail:: /img/graphql/cloud/getting-started/create-project-existing-db.png
   :alt: Create project for existing database
   :width: 552px
   :group: create
   :class: inline-block

Step 4: Try Hasura out
----------------------

Click ``Launch Console`` to open the Hasura console in your browser and
:ref:`make your first GraphQL query <first_graphql_query>` or :ref:`set up your first event trigger <first_event_trigger>`.

.. thumbnail:: /img/graphql/cloud/getting-started/project-launch-console.png
  :alt: Project actions
  :width: 860px

You can navigate to the ``Pro`` tab in the console to check out the Pro features that Hasura Cloud has set up for you.

.. thumbnail:: /img/graphql/cloud/metrics/pro-tab-overview.png
  :alt: Hasura Console: Pro tab
  :width: 1000px

Next steps
----------

You can check out our `30-Minute Hasura Basics Course <https://hasura.io/learn/graphql/hasura/introduction/>`__
and other `GraphQL & Hasura Courses <https://hasura.io/learn/>`__ for a more detailed introduction to Hasura.

You can also click the gear icon to manage your Hasura Cloud project. (e.g. add :ref:`collaborators <manage_project_collaborators>`,
:ref:`env vars <manage_project_env_vars>` or :ref:`custom domains <manage_project_domains>`) and :ref:`add an admin secret <secure_project>`
to make sure that your GraphQL endpoint and the Hasura console are not publicly accessible.

.. thumbnail:: /img/graphql/cloud/getting-started/project-manage.png
  :alt: Project actions
  :width: 860px

