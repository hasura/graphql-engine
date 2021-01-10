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

Step 1: Project setup
---------------------

To begin, navigate to the ``Projects`` page, and click the ``New Project`` link.

.. thumbnail:: /img/graphql/cloud/projects/create-new-project.png
   :alt: create new project button
   :width: 400

This opens a form on the right where you can create your new project.

Enter the project name, choose a pricing plan and select a region for your project in this form.

.. thumbnail:: /img/graphql/cloud/projects/project-setup.png
   :alt: project setup

Once you have completed the project setup, continue to the database setup.

Step 2: Database setup
----------------------

New database
^^^^^^^^^^^^

Hasura Cloud does not host databases, but does provide integrations with which you can create databases on managed cloud providers like Heroku. Integrations for AWS, GCP, and Azure are coming soon.

To get started, click ``Try with Heroku``, and follow the prompts to authenticate with Heroku. Hasura Cloud will integrate with your Heroku account and manage the initial setup of a dev-tier Postgres instance. You can always upgrade the instance and manage options later through your Heroku account.

.. thumbnail:: /img/graphql/cloud/projects/project-new-database-setup.png
   :alt: database setup with new database

Existing database
^^^^^^^^^^^^^^^^^

To connect an existing Postgres database to your new project, click ``Enter Database URL``, and enter your database connection string (looks like ``postgres://username:password@hostname:port/dbname``).

.. thumbnail:: /img/graphql/cloud/projects/project-existing-database-setup.png
   :alt: database setup with existing database

Allowing connections from Hasura Cloud IP
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For some cloud services, like GCP, you'll need to adjust your Postgres connection settings to allow connections from the Hasura Cloud IP address. You can copy the IP address from either the copy icon in the ``Database Setup`` , or the ``Hasura Cloud IP`` field on the project's details view. You may also need to disable SSL. Adding custom cert information to a Hasura Cloud instance is not yet available. 

.. thumbnail:: /img/graphql/cloud/projects/project-create-hasura-cloud-ip.png
   :alt: Existing database setup
   :width: 500px

.. thumbnail:: /img/graphql/cloud/projects/hasura-cloud-ip.png
   :alt: Hasura Cloud IP field
   :width: 1000px

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

More databases
--------------

Support for more databases (MySQL, SQL Server etc) is coming soon.
