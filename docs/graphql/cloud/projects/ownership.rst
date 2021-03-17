.. meta::
   :description: Transferring Ownership on Hasura Cloud
   :keywords: hasura, docs, project, team, ownership transfer

.. _project_ownership_transfer:

Project ownership transfer
==========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

You can transfer ownership of your project to another user. After the ownership is transferred successfully, the original 
owner will lose all access to the project.

.. note::

   If the project is on the ``Standard`` (pay-as-you-go) tier, the new owner will pay for the entire data usage for the month in which the ownership is transferred.

Send an ownership transfer invitation
-------------------------------------

Go to the project settings page and scroll to the bottom. Enter the email of the user you want to transfer the project ownership to.

.. thumbnail:: /img/graphql/cloud/projects/transfer-ownership.png
   :alt: Transfer Project Ownership
   :width: 1146px

Revoke an ownership transfer invitation
---------------------------------------

To revoke the ownership transfer invitation, click on the ``Revoke`` button right next to the email of the invitee. 

.. thumbnail:: /img/graphql/cloud/projects/remove-ownership.png
   :alt: Remove transfer ownership invite
   :width: 1146px

Resend an ownership transfer invitation
---------------------------------------

To resend the ownership transfer invitation, click on the resend symbol right next to the email of the invitee. 

.. thumbnail:: /img/graphql/cloud/projects/resent-ownership.png
   :alt: Resend transfer ownership invite
   :width: 1146px


Invitations
-----------

You can see the projects that you have been invited to become the owner of, on the project listing page.

.. thumbnail:: /img/graphql/cloud/projects/project-ownership-invitation.png
   :alt: Projects invited to own
   :width: 1146px

You can accept an invitation by clicking on the ``Accept`` button. 

.. note::

   If the project is on the ``Standard`` (pay-as-you-go) tier, the new owner must have an active card
   associated with their Hasura Cloud account to accept the invitation.
