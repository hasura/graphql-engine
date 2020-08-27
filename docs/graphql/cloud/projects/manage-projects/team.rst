.. meta::
   :description: Managing teams on Hasura Cloud
   :keywords: hasura, docs, project, team

.. _manage_project_team:

Team tab
========

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

The ``Team`` tab shows the current people with access to the project. 

Add a collaborator
------------------

Click ``New Collaborator`` to invite a new team member.

.. thumbnail:: /img/graphql/cloud/projects/team-view.png
   :alt: Team tab
   :width: 1146px

Team roles
^^^^^^^^^^

Team members can have different levels of access in the Hasura console.

- **Admin** has complete access to all project tools and configurations.
- **User** has limited privileges:

  - The ``Execute GraphQL`` permission allows running queries, mutations, and subscriptions.
  - The ``View Metrics`` permission allows inspecting operation data and the performance dashboard.

.. thumbnail:: /img/graphql/cloud/projects/add-collaborator.png
   :alt: Add collaborator
   :width: 437px

.. note::

  Invitations can be accepted or declined via the ``Invitations`` tab of your Hasura Cloud settings.
