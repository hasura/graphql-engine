.. meta::
   :description: Creating projects on Hasura Cloud
   :keywords: hasura, cloud, docs, start

.. _create_project:

Creating projects
=================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

You can create a new Hasura Cloud project with either a new Postgres database, or an existing Postgres database with a publicly available IP address.

To begin, navigate to the ``Projects`` page, and click the ``New Project`` link.

.. thumbnail:: /img/graphql/cloud/projects/create-new-project.png
   :alt: create new project button
   :width: 400

Creating a project with a new database
--------------------------------------

Hasura Cloud does not host databases, but does provide integrations with which you can create databases on managed cloud providers like Heroku. Integrations for AWS, GCP, and Azure are coming soon.

To get started, click ``Try with Heroku``, and follow the prompts to authenticate with Heroku. Hasura Cloud will integrate with your Heroku account and manage the initial setup of a dev-tier Postgres instance. You can always upgrade the instance and manage options later through your Heroku account.

.. thumbnail:: /img/graphql/cloud/projects/create-project-heroku.png
   :alt: create new hosted project

Creating a project with an existing database
--------------------------------------------

To create a new Hasura Cloud project connected to an existing Postgres database, click ``Enter Database URL``, and enter your database connection string (looks like ``postgres://username:password@hostname:port/dbname``).

.. thumbnail:: /img/graphql/cloud/projects/new-project-hosted.png
   :alt: create new hosted project

Allowing connections from Hasura Cloud IP
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For some cloud services, like GCP, you'll need to adjust your Postgres connection settings to allow connections from the Hasura Cloud IP address. You can copy the IP address from either the copy icon in the ``Database Setup`` window, or the ``Hasura Cloud IP`` field on the project's details view. You may also need to disable SSL. Adding custom cert information to a Hasura Cloud instance is not yet available. 

.. thumbnail:: /img/graphql/cloud/projects/existing-db-setup.png
   :alt: Existing database setup
   :width: 568px

.. thumbnail:: /img/graphql/cloud/projects/hasura-cloud-ip.png
   :alt: Hasura Cloud IP field
   :width: 1200px

.. thumbnail:: /img/graphql/cloud/projects/gcp-postgres-authorized-network.png
   :alt: whitelist Hasura instance IP in Postgres settings
   :width: 727px

Connecting to a database not exposed over the internet
------------------------------------------------------

`Contact us <https://hasura.io/contact-us/>`__ for VPC peering and on-premise solutions.

.. _cloud_postgres_permissions:

Postgres permissions
--------------------

Hasura Cloud works with **Postgres versions 9.5 and above**.

If you’re running in a controlled environment, you might need to configure
Hasura Cloud to use a specific Postgres user that your DBA gives you.

Apart from the :ref:`Hasura Core Postgres permissions <postgres_permissions>`,
Hasura Cloud needs the following extra permissions:

- (required) Read and write access to ``hdb_pro_catalog`` schema.

.. code-block:: sql

   -- execute these statements after executing the ones mentioned in Hasura Core docs
   -- create the schemas required by the hasura cloud system
   CREATE SCHEMA IF NOT EXISTS hdb_pro_catalog;
   
   -- make the user an owner of system schemas
   ALTER SCHEMA hdb_pro_catalog OWNER TO hasurauser;

