.. meta::
   :description: Managing teams in Hasura Cloud
   :keywords: hasura, docs, cloud, teams

.. _projects:

Projects & collaborators
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The ``Projects`` page show a list of your projects.

.. thumbnail:: /img/graphql/cloud/projects/projects-list.png
   :alt: Projects list
   :width: 1200px

For each project, you can do the one of the following actions:

.. thumbnail:: /img/graphql/cloud/projects/project-actions.png
   :alt: Project actions
   :width: 860px

- Click ``Launch Console`` to open the Hasura console in your browser. The ``Pro`` tab
  lets you use the Pro features that Hasura Cloud has set up for you.

  .. thumbnail:: /img/graphql/cloud/metrics/pro-tab-overview.png
     :alt: Hasura Console: Pro tab
     :width: 900px

- Click the gear icon to manage your project

  .. thumbnail:: /img/graphql/cloud/projects/project-details.png
     :alt: General tab
     :width: 900px

.. note::

   Please see the :ref:`API reference <cloud_api_reference>` to create and manage Hasura Cloud projects programmatically.

Dig deeper
----------

.. toctree::
   :maxdepth: 1
   :titlesonly:

   Creating projects <create>
   Project Details <details>
   Project Collaborators <collaborators>
   Project Env vars <env-vars>
   Project Domains <domains>
   Securing projects <secure>
   Switching pricing plans <pricing>
   Heroku database URL Sync <heroku-url-sync>
   Deleting projects <delete>
   
