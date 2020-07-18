.. meta::
   :description: Manage a project on Hasura Cloud
   :keywords: hasura, docs, project

.. _manage_project:

Manage a project
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

You can manage project teams and environment variables directly from the Hasura Cloud account. Click on the project's "Project details" to access the following admin tabs:

General
-------

Your project's general details are provided here, including:

- **Project name**: unique auto-generated name
- **Project Id**: unique auto-generated ID
- **Owner**: the email of the project owner (creator)
- **End point**: the base URL of the project

.. thumbnail:: /img/cloud/projects/project-details.png
   :alt: project details page

Team
----

The teams tab shows the current people with access to the Hasura Cloud project:

.. thumbnail:: /img/cloud/projects/team-view.png
   :alt: project team list

Team roles
^^^^^^^^^^

Team members can have different levels of access in the Console:

**User**: limited privileges

* Execute GraphQL: can run queries / mutations / subscriptions
* View metrics: can inspect operation data and performance dashboard

**Admin**: complete access to all project tools and configurations

Adding a team member
^^^^^^^^^^^^^^^^^^^^

You can invite people to your project with a specific role:

.. thumbnail:: /img/cloud/projects/cloud-add-collaborator.png
   :alt: add to team options

Invitations are sent via email with a link to accept the invitation. Clicking the link brings them to the Hasura Cloud login page. If they don’t already have an account, they can sign-up to create a new account. 

.. admonition:: Invitation email must match account email

  If a new account is created, make sure the invitation email matches to the account email.

Invitations can be accepted or declined via the Invitations tab of your Hasura Cloud Settings:

.. thumbnail:: /img/cloud/projects/project-collaboration-invitation.png
   :alt: accept or decline an invitation
   :width: 400

ENV Vars
--------

You can also manage the `Hasura GraphQL Engine environment variables 
<https://hasura.io/docs/1.0/graphql/manual/deployment/graphql-engine-flags/reference.html#command-flags>`__ or add your own custom environment variables in the Env Vars section:

.. thumbnail:: /img/cloud/projects/add-env-var.png
   :alt: add env var options