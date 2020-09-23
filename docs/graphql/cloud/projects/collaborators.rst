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

You can invite collaborators to your project and grant them partial or complete access to your Hasura console. The ``Collaborators`` tab shows the current people who have access to the project and the people who have been invited to the project.

Invite a collaborator
---------------------

Click ``Invite a Collaborator`` to invite a new collaborator by their email.

.. thumbnail:: /img/graphql/cloud/projects/collaborators-view.png
   :alt: Collaborators tab
   :width: 1146px

Collaborator roles
^^^^^^^^^^^^^^^^^^

Collaborators can have different levels of access in the Hasura console.

- **Admin** has complete access to the console i.e they can change the schema and the GraphQL engine metadata.
- **User** has limited privileges:

  - The ``Execute GraphQL`` permission allows running queries, mutations and subscriptions from the ``GraphiQL`` tab of the console.
  - The ``View Metrics`` permission allows inspecting operation data and metrics from the ``PRO`` tab of the console.

.. thumbnail:: /img/graphql/cloud/projects/add-collaborator.png
   :alt: Add collaborator
   :width: 437px

Remove a collaborator
---------------------

To remove a collaborator, click on the collaborator and then click on the remove icon on the top right:


.. thumbnail:: /img/graphql/cloud/projects/remove-collaborator.png
   :alt: Remove collaborator
   :width: 437px

If you have invited a collaborator, you can click on the ``Invited`` button to resend or revoke the invitation.

.. thumbnail:: /img/graphql/cloud/projects/revoke-collaboration-invitation.png
   :alt: Revoke collaboration invitation
   :width: 1146px


Invitations
-----------

You can see the projects that you have been invited to collaborate on, on the project listing page.

.. thumbnail:: /img/graphql/cloud/projects/project-collaboration-invitation.png
   :alt: Projects invited to collaborate
   :width: 1146px
