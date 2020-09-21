.. meta::
   :description: Managing collaborators on Hasura Cloud
   :keywords: hasura, docs, project, team, collaborators

.. _manage_project_collaborators:

Project Collaborators
=====================

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

.. thumbnail:: /img/graphql/cloud/projects/collaborators-view.png
   :alt: Collaborators tab
   :width: 1146px

Collaborator roles
^^^^^^^^^^^^^^^^^^

Collaborators can have different levels of access in the Hasura console.

- **Admin** has complete access to all project tools and configurations.
- **User** has limited privileges:

  - The ``Execute GraphQL`` permission allows running queries, mutations, and subscriptions.
  - The ``View Metrics`` permission allows inspecting operation data and the performance dashboard.

.. thumbnail:: /img/graphql/cloud/projects/add-collaborator.png
   :alt: Add collaborator
   :width: 437px

.. note::

  Invitations can be accepted or declined via the ``Invitations`` tab of your Hasura Cloud settings.
