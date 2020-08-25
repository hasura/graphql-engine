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
- To create a new database, choose ``Try a free database with Heroku``.

.. thumbnail:: /img/graphql/cloud/getting-started/connect-db.png
   :alt: Connect new or existing database
   :width: 591px

Step 2a: Enter database URL (for existing database)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you chose ``I have an existing Postgres database`` in :ref:`Step 2 <cloud_connect_db>`, enter a database URL.

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

Next steps
----------

Once you've created your project, you can get started with building with Hasura or manage your project.

.. contents::
  :backlinks: none
  :depth: 1
  :local:

.. thumbnail:: /img/graphql/cloud/getting-started/project-functionalities.png
  :alt: Project actions
  :width: 860px


Explore the Hasura console
^^^^^^^^^^^^^^^^^^^^^^^^^^

Click ``Launch Console`` to open the Hasura console in your browser and :ref:`make your first GraphQL query <first_graphql_query>` or :ref:`set up your first event trigger <first_event_trigger>`.

You can navigate to the ``Pro`` tab to check out the Pro features that Hasura Cloud has set up for you.

.. thumbnail:: /img/graphql/cloud/metrics/pro-tab-overview.png
   :alt: Hasura Console: Pro tab
   :width: 1118px

Manage your project
^^^^^^^^^^^^^^^^^^^

Click the gear icon to :ref:`manage your project <manage_project>` (e.g. add collaborators, env vars or custom domains).

Add an admin secret
^^^^^^^^^^^^^^^^^^^

:ref:`Add an admin secret <secure_project>` to make sure that your GraphQL endpoint and the Hasura console are not publicly accessible.