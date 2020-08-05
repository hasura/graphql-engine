.. meta::
   :description: Managing projects on Hasura Cloud
   :keywords: hasura, docs, project

.. _manage_project:

Managing projects
=================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

To view a project's details, and manage its teams and environment variables, navigate to the ``Projects`` page and click the gear icon on the project.

.. thumbnail:: /img/cloud/projects/projects-manage.png
   :alt: Manage project
   :width: 865px

**General** tab
---------------

This tab shows the project's general details, including:

- **Name**: Unique auto-generated name for the project
- **ID**: Unique auto-generated ID for the project
- **GraphQL API**: GraphQL endpoint for the project
- **Admin Secret**: Secret for securing the GraphQL endpoint
- **Owner**: Email of the project owner

.. thumbnail:: /img/cloud/projects/project-details.png
   :alt: General tab
   :width: 1163px

**Team** tab
------------

This tab shows the current people with access to the project. Click ``New Collaborator`` to invite a new team member.

.. thumbnail:: /img/cloud/projects/team-view.png
   :alt: Team tab
   :width: 1146px

Team roles
^^^^^^^^^^

Team members can have different levels of access in the Hasura console.

- **Admin** has complete access to all project tools and configurations.
- **User** has limited privileges:

  - The ``Execute GraphQL`` permission allows running queries, mutations, and subscriptions.
  - The ``View Metrics`` permission allows inspecting operation data and the performance dashboard.

.. thumbnail:: /img/cloud/projects/add-collaborator.png
   :alt: Add collaborator
   :width: 437px

.. note::

  Invitations can be accepted or declined via the ``Invitations`` tab of your Hasura Cloud settings.


**Env vars** tab
----------------

This tab shows `Hasura GraphQL Engine environment variables 
<https://hasura.io/docs/1.0/graphql/manual/deployment/graphql-engine-flags/reference.html#command-flags>`__. Click ``New Env Var`` to add your own custom environment variables.

.. thumbnail:: /img/cloud/projects/add-env-var.png
   :alt: add env var options
   :width: 1200px
