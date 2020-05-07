.. meta::
   :description: Instructions to migrate your graph.cool project to Hasura
   :keywords: hasura, docs, guide, migration, graph.cool

.. _graphcool_migration:

Migrate from Graph.cool to Hasura
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

If you are looking to migrate your Graph.cool project to Hasura, these are the steps that you will need to roughly follow.

.. note::

   This guide is not comprehensive and some steps require manual intervention.

Step 1: Export data from Graph.cool
-----------------------------------

Export your Graph.cool data using their `export tool <https://export.graph.cool>`__. This will give a MySQL binary dump of your current data.

.. thumbnail:: ../../../../img/graphql/manual/guides/graphcool-export.png
   :alt: Graph.cool Export

Step 2: Set up an intermediary MySQL server
-------------------------------------------

We need to set up a MySQL server as an intermediary step in order to migrate data from Graph.cool to Postgres.

.. contents::
  :backlinks: none
  :depth: 1
  :local:

Step 2.1: Start MySQL with Docker
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   docker run --name graphcool-mysql -e MYSQL_ROOT_PASSWORD=my-secret-pw -p 3306:3306 -d mysql:latest --default-authentication-plugin=mysql_native_password

Replace the password as required.

Step 2.2: Connect to MySQL via mysql CLI
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   mysql --user=root --password=my-secret-pw --host=<host>

Replace the host and password as required.

Step 2.3: Create a database in MySQL
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   create database public;

Step 2.4: Import data
^^^^^^^^^^^^^^^^^^^^^

Import Graph.cool's MySQL export into your local MySQL instance:

.. code-block:: bash

   mysql --user=root --password=my-secret-pw --host=<host> public --binary-mode=1 < <pathtomysqlexport>

Replace host and pathtomysqlexport as required. Your data should now be present in the MySQL database.

Step 3: Migrate data to Hasura
------------------------------

Since MySQL is now set up with all the data from Graph.cool, we need to create a Hasura and Postgres instance, and to import the data to the same.

.. contents::
  :backlinks: none
  :depth: 1
  :local:

Step 3.1: Set up Hasura
^^^^^^^^^^^^^^^^^^^^^^^

Refer to the :ref:`Getting started <docker_simple>` guide to set up Hasura using ``docker-compose``.

Step 3.2: Import data into Postgres
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We will use ``pgloader`` to migrate from MySQL to Postgres. Refer to their `installation guide <https://github.com/dimitri/pgloader>`__ for setting this up.

Once you have installed, execute the following command:

.. code-block:: bash

   pgloader mysql://root:my-secret-pw@<host>/public postgresql://postgres:postgrespassword@<host>:5432/postgres

Replace ``<host>`` as required.

Your data should now be present in the Postgres database.

Step 3.3: Connect Hasura to Postgres
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Once the dataset is migrated to Postgres, Hasura should be able to track tables and relationships. 

.. note::

   If you have enums in your Graph.cool project, check out :ref:`this page <enums_graphql_engine>`, since they're handled differently in Hasura. 

Step 4: Migrate structure & functionality
-----------------------------------------

After migrating the data to Hasura, there is some manual work involved in migrating the structure and functionality of your Graph.cool project.

.. contents::
  :backlinks: none
  :depth: 1
  :local:

Step 4.1: Restructure connection tables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can rename tables/columns to match your client-side queries as required. 
Do note that, for every one-to-one relationship, Graph.cool would have created a connection table to link them. This would require a bit of manual work to restructure. 
Currently, there is no automation available for this step. Carefully review the connection tables and make the necessary changes.

Step 4.2: Migrate functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

In case you have functions in Graph.cool, Hasura has an equivalent feature called :ref:`event triggers <event_triggers>`. Migrating this involves taking your code and deploying it on a different platform (preferably serverless functions).

Do note that for event triggers, the payload that Hasura sends might be different, and you might have to change the way the request body parameters are handled in your function code.

Step 4.3: Migrate auth
^^^^^^^^^^^^^^^^^^^^^^

There are two ways of authenticating users in Graph.cool:

1. Using Auth0
2. Using email-password auth.

If you were using Auth0 with Graph.cool, the migration should be fairly straightforward. You can configure Hasura with Auth0 easily by following :ref:`this guide <guides_auth0_jwt>`.

In case you are using email-password auth, Graph.cool generates mutations for 

- creating a user ``createUser(authProvider: { email: { email, password } })`` and 
- login ``signinUser(email: { email, password })``. 

You will need to implement these custom mutations using :ref:`Hasura actions <actions>`. 
Refer to this example for a `custom signup mutation <https://github.com/hasura/hasura-actions-examples/tree/master/auth>`__.

Step 4.4: Migrate permissions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The CRUD permissions in Graph.cool can be manually migrated to Hasura's permission system. You can define roles in Hasura and configure permissions declaratively for all the CRUD operations. 
Refer to :ref:`this page <authorization>` for configuring Hasura permissions.

