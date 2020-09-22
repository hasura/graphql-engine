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

You can invite collaborators to your project and grant them partial or complete access to your Hasura console.

Invite a collaborator
---------------------

Click ``Invite a Collaborator`` to invite a new collaborator by their email.

.. thumbnail:: /img/graphql/cloud/projects/collaborators-view.png
   :alt: Collaborators tab
   :width: 1146px

Collaborator roles
^^^^^^^^^^^^^^^^^^

Collaborators can have different levels of access in the Hasura console.

- **Admin** has complete access to the console i.e they can change the schema and the GraphQL Engine metadata.
- **User** has limited privileges:

  - The ``Execute GraphQL`` permission allows running queries, mutations, and subscriptions from the ``GraphiQL`` tab of the console.
  - The ``View Metrics`` permission allows inspecting operation data and metrics from the ``PRO`` tab of the console.

.. thumbnail:: /img/graphql/cloud/projects/add-collaborator.png
   :alt: Add collaborator
   :width: 437px


Invitations
-----------

You can see the projects that you have been invited to collaborate on, on the project listing page.

.. thumbnail:: /img/graphql/cloud/projects/project-collaboration-invitation.png
   :alt: Projects invited to collaborate
   :width: 1146px
