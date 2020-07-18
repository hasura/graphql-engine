.. meta::
   :description: Create a new project on Hasura Cloud
   :keywords: hasura, docs, start

.. _create_project:

Create a new project
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:


Create a new project
--------------------

You can create a new Hasura Cloud project with either a new PostgreSQL database or any of your existing PostgreSQL databases with a publicly availably IP address

.. thumbnail:: /img/cloud/projects/create-new-project.png
   :alt: create new project button
   :width: 400


Create new project with new database on Heroku
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Hasura Cloud does not host databases, but we do provide integrations using which you can create databases on managed cloud providers like Heroku. The Heroku integration is live right now and we'll be adding AWS/GCP/Azure ones soon.

To start from scratch, Hasura Cloud will integrate with your Heroku account and manage the initial setup of a dev-tier PostgreSQL instance. You can always upgrade the instance and manage options later through your Heroku account.

.. thumbnail:: /img/cloud/projects/create-project-heroku.png
   :alt: create new hosted project


Create new project with an existing Postgres database
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To create a new Hasura Cloud project connected to an existing PostgreSQL database, you simply need the database connection string (something like ``postgres://username:password@hostname:port/dbname``)

.. thumbnail:: /img/cloud/projects/new-project-hosted.png
   :alt: create new hosted project

For some cloud services, for example GCP, you'll need to adjust your PostgreSQL connection settings. Authorize the Hasura engine IP address ("database connection IP" on the project's details view), and possibly also disable SSL; adding custom cert information to a Hasura Cloud instance is not yet available. 

.. thumbnail:: /img/cloud/projects/project-details.png
   :alt: project details includes database connection IP
   :width: 400

.. thumbnail:: /img/cloud/projects/gcp-postgres-authorized-network.png
   :alt: whitelist Hasura instance IP in Postgres settings
   :width: 400


Connect to database not exposed over internet
---------------------------------------------

`Contact Hasura <https://hasura.io/contact-us/>`__ for VPC peering and on-premise solutions. 
